#' LoadBelgium
#'
#' @description Reads in subnational data for Belgium to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data was obtained from Sciensano, the Belgian institute for health: \url{https://epistat.wiv-isp.be/covid/}. We note that this function needs to read files corresponding to daily reports (some of which may not exist),
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Belgium <- LoadBelgium()
#' }
#' @seealso [LoadData()]
#' @export
LoadBelgium <- function() {
  # Sciensano, the Belgian institute for health: https://epistat.wiv-isp.be/covid/
  geomBelgium <- pop_belgium <- names_belgium <- NULL

  utils::data("geomBelgium", "names_belgium", "pop_belgium", envir = environment())
  geomBelgium <- sf::st_as_sf(geomBelgium)

  getDate <- function(x, y) {
    dates <- as.character(Sys.Date() - x)
    if (y == 0) {
      string <- unlist(strsplit(dates, "-"))
      new_date <- sprintf("%s%s%s", string[1], string[2], string[3])
    } else {
      new_date <- dates
    }
    return(new_date)
  }

  # cases - loading individual files from historical datasets of cumulative cases at municipality level (581), which we will aggregate to Arrondissements (43).
  # find latest data
  flag <- 0
  aa <- 0
  latest_data <- NULL

  while (flag == 0) {
    STRING <- paste0("https://epistat.sciensano.be/Data/", getDate(aa, 0), "/COVID19BE_CASES_MUNI_CUM_", getDate(aa, 0), ".csv")
    tryCatch(
      {
        latest_data <- vroom::vroom(STRING, show_col_types = FALSE, progress = FALSE)
      },
      error = function(cond) {
        #warning(paste0("No data for ", getDate(aa, 0)))
      }
    )
    
    if (is.null(latest_data) == FALSE) {
      flag <- 1
    } else {
      aa <- aa + 1
    }
    if (aa > 30) {
      stop("no recent data1")
      flag <- 2
    }
  }

  UpdateDate <- getDate(aa, 1)
  # find past data
  flag <- 0
  past_data <- NULL

  while (flag == 0) {
    STRING <- paste0("https://epistat.sciensano.be/Data/", getDate(14 + aa, 0), "/COVID19BE_CASES_MUNI_CUM_", getDate(14 + aa, 0), ".csv")
    tryCatch(
      {
        past_data <- vroom::vroom(STRING, show_col_types = FALSE, progress = FALSE)
      },
      error = function(e) {
        #warning(paste0("No data for ", getDate(14 + aa, 0)))
      }
    )
    if (is.null(past_data) == FALSE) {
      flag <- 1
    } else {
      aa <- aa + 1
    }
    if (aa > 30) {
      stop("no recent data2")
      flag <- 2
    }
  }
  PastDate <- getDate(14 + aa, 1)
  DateDiff <- as.numeric(abs(diff(c(as.Date(UpdateDate), as.Date(PastDate))))) # difference in days between case records

  # select columns of interest - NL and FR Arrondissement names and cumulative cases per municipality.
  latest_data <- latest_data[, c("TX_ADM_DSTR_DESCR_NL", "TX_ADM_DSTR_DESCR_FR", "CASES")]
  past_data <- past_data[, c("TX_ADM_DSTR_DESCR_NL", "TX_ADM_DSTR_DESCR_FR", "CASES")]

  # get the list of arrondissment:
  Arrondissement <- unique(latest_data$TX_ADM_DSTR_DESCR_NL) # 43 Arrondissements and NA

  # collect data for each arrondissment and assemble them into 1 table
  getData <- function(dataSet) {
    updated_data <- data.frame(Arrondissement = as.character(), Cases = as.numeric())
    for (i in 1:length(Arrondissement)) {
      data <- dataSet %>% dplyr::filter(dataSet$TX_ADM_DSTR_DESCR_NL == Arrondissement[i])
      if (sum(data$CASES == "<5") > 0) {
        data[data$CASES == "<5", "CASES"] <- 0 ## assume there is no case when the box shows <5
      }
      temp <- data.frame(Arrondissement = unlist(strsplit(Arrondissement[i], "Arrondissement "))[2], Cases = sum(as.numeric(data$CASES)))
      updated_data <- rbind(updated_data, temp)
    }
    return(updated_data)
  }

  latest_update <- getData(latest_data)
  past_update <- getData(past_data)
  difference <- (latest_update$Cases - past_update$Cases) * 10 / DateDiff

  ### DESIRED TABLE
  finalData <- data.frame(Arrondissement = latest_update[, "Arrondissement"], Difference = difference)

  # geojson file
  # geomBelgium <- st_read('countries/data/geom/geomBelgium.geojson')
  # geomBelgium <- geomBelgium[,c("micro_name","geometry")]
  # for (i in 1:length(geomBelgium$micro_name)){
  #  geomBelgium$micro_name[i] <- unlist(strsplit(geomBelgium$micro_name[i], "Arrondissement "))[2]
  # }
  # geomBelgium$micro_name[28] <- sort(finalData$Arrondissement)[22] # La Louviere
  # geomBelgium$micro_name[39] <- sort(finalData$Arrondissement)[29] # Neufch?teau
  # geomBelgium <- st_write(geomBelgium,'countries/data/geom/geomBelgium.geojson')

  finalData <- dplyr::inner_join(geomBelgium, finalData, by = c("micro_name" = "Arrondissement"))

  # population

  # add to dataset
  belgiumdf <- dplyr::inner_join(finalData, pop_belgium, by = c("micro_name" = "Name"))


  # Change names to account for NL and FR:
  nameMuni <- names_belgium %>%
    dplyr::rename_with(.fn = \(x) stringr::str_replace_all(x, "\\s", "\\."))

  for (i in 1:length(belgiumdf$micro_name)) {
    for (j in 1:length(nameMuni$Dutch.name)) {
      if (belgiumdf$micro_name[i] == nameMuni$Dutch.name[j] & nameMuni$French.name[j] != "-") {
        belgiumdf$micro_name[i] <- paste0(belgiumdf$micro_name[i], "/", nameMuni$French.name[j])
      } else if (belgiumdf$micro_name[i] == nameMuni$French.name[j] & nameMuni$Dutch.name[j] != "-") {
        belgiumdf$micro_name[i] <- paste0(belgiumdf$micro_name[i], "/", nameMuni$Dutch.name[j])
      } else if ((belgiumdf$micro_name[i] == nameMuni$Dutch.name[j] | belgiumdf$micro_name[i] == nameMuni$French.name[j]) & (nameMuni$French.name[j] == "-" | nameMuni$Dutch.name[j] == "-")) {
        belgiumdf$micro_name[i] <- belgiumdf$micro_name[i]
      }
    }
    if (belgiumdf$micro_name[i] == "Tournai-Mouscron") {
      belgiumdf$micro_name[i] <- paste0("Tournai/Doornik-Mouscron/Moeskroen")
    }
  }

  # integrate datsets
  belgiumdf$RegionName <- paste(belgiumdf$micro_name, belgiumdf$country_name, sep = ", ")
  belgiumdf$Country <- belgiumdf$country_name
  belgiumdf$DateReport <- as.character(UpdateDate)
  belgiumdf$pInf <- belgiumdf$Difference / belgiumdf$Population

  BELGIUM_DATA <- subset(belgiumdf, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(BELGIUM_DATA)
}
