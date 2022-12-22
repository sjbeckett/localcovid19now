#' getDataND
#'
#' @param code Municipality code.
#'
#' @return COVID-19 data for the Netherlands. Used in LoadNetherlands().
#' @keywords internal
getDataND <- function(Mcode, netherlandsData) {
  Code <- NULL
  temp <- netherlandsData %>%
    dplyr::filter(Code == Mcode)
  temp$CumSum <- cumsum(temp$Cases)
  today <- temp$Date[length(temp$Date)]
  past_date <- today - 14
  pastData <- temp[temp$Date <= past_date, ]
  ### SOME ROWS DO NOT REPORT MUNICIPALITY NAME
  difference <- (temp$CumSum[length(temp$CumSum)] - pastData$CumSum[length(pastData$CumSum)]) / 14 * 10
  vec <- data.frame(Municipality = temp$Municipality[1], Code = Mcode, Date = today, Difference = difference)
  return(vec)
}

#' LoadNetherlands
#'
#' @description Reads in subnational data for Netherlands to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Covid-19 numbers per municipality as of publication date. RIVM / I & V / EPI. OSIRIS General Infectious Diseases (AIZ). \url{https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/5f6bc429-1596-490e-8618-1ed8fd768427?tab=general}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Netherlands <- LoadNetherlands()
#' }
#' @seealso [LoadData()]
#' @export
LoadNetherlands <- function() {
  pop_netherlands <- NULL

  # note that the underlying geometry of reporting has changed multiple times during the pandemic due to changes in municipality boundaries.

  # utils::data("geomNetherlands", envir = environment())
  geomNetherlands <- sf::st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2022_gegeneraliseerd&outputFormat=json", quiet = TRUE)
  geomNetherlands <- sf::st_transform(geomNetherlands, crs = 4326)
  geomNetherlands$micro_code <- geomNetherlands$statcode
  # also need to combine Weesp with Amsterdam (March 2022)
  HMM <- sf::st_union(geomNetherlands[c(which(geomNetherlands$statnaam == "Weesp"), which(geomNetherlands$statnaam == "Amsterdam")), ]) %>% sf::st_cast("MULTIPOLYGON")
  geomNetherlands$geometry[which(geomNetherlands$statnaam == "Amsterdam")] <- HMM

  # population: https://opendata.cbs.nl/statline/#/CBS/nl/dataset/70072ned/table?dl=5A35F
  utils::data("pop_netherlands", envir = environment())


  # Covid-19 numbers per municipality as of publication date. RIVM / I & V / EPI. OSIRIS General Infectious Diseases (AIZ). https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/5f6bc429-1596-490e-8618-1ed8fd768427?tab=general

  NLdata <- vroom::vroom("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv", delim = ";", show_col_types = FALSE, progress = FALSE) %>%
    dplyr::select(Date = "Date_of_publication", Code = "Municipality_code", Municipality = "Municipality_name", Province = "Province", Cases = "Total_reported")

  municipalities <- unique(NLdata$Code)
  municipalities <- municipalities[-which(is.na(municipalities))] # remove NA values.

  netherlandsTable <- data.frame()
  for (ii in 1:length(municipalities)) {
    vec <- getDataND(municipalities[ii], NLdata)
    netherlandsTable <- rbind(netherlandsTable, vec)
  }

  # Note that geomNetherlands$Bevolkingsaantal is population size.

  netherlandsMap <- dplyr::inner_join(geomNetherlands, netherlandsTable, by = c("micro_code" = "Code")) %>%
    dplyr::inner_join(pop_netherlands, by = c("Municipality" = "Gemeente"))
  # dplyr::inner_join(pop_netherlands, by = c("micro_name" = "Name"))

  # netherlandsMap$RegionName <- paste(netherlandsMap$micro_name, netherlandsMap$macro_name, netherlandsMap$country_name, sep = ", ")
  # netherlandsMap$Country <- netherlandsMap$country_name
  netherlandsMap$Country <- "Netherlands"
  netherlandsMap$RegionName <- paste(netherlandsMap$Municipality, "Netherlands", sep = ", ")
  netherlandsMap$DateReport <- as.character(netherlandsMap$Date)
  netherlandsMap$pInf <- netherlandsMap$Difference / netherlandsMap$Population
  netherlandsMap$geoid <- paste0("NL_", netherlandsMap$micro_code)

  NETHERLANDS_DATA <- subset(netherlandsMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf"))

  return(NETHERLANDS_DATA)
}
