#' dataQueryUK
#'
#' @param date Get COVID-19 data for this date.
#'
#' @return COVID-19 data for UK on this date. Used in LoadUK().
#' @keywords internal
dataQueryUK <- function(date) {
  dataURL <- paste0("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;date=", date, '&structure={"date":"date","code":"areaCode","cases":"cumCasesBySpecimenDate"}')
  response <- httr::GET(
    url = dataURL,
    httr::timeout(10)
  )
  if (response$status_code >= 400) {
    err_msg <- httr::http_status(response)
    stop(err_msg)
  } else if (response$status_code >= 204) {
    dmod <- 1
    while (response$status_code >= 204) {
      cur_date <- date - dmod
      dataURL <- paste0("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;date=", cur_date, '&structure={"date":"date","code":"areaCode","cases":"cumCasesBySpecimenDate"}')
      response <- httr::GET(
        url = dataURL,
        httr::timeout(10)
      )
      dmod <- dmod + 1
    }
  }

  # Convert response from binary to JSON:
  json_text <- httr::content(response, "text")
  data <- jsonlite::fromJSON(json_text)$data %>%
    dplyr::mutate(date = lubridate::as_date(date))
  return(data)
}

#' LoadUK
#'
#' @description Reads in subnational data for the United Kingdom to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' The COVID-19 data is from the UK API from Public Health England and NHSX: \url{https://coronavirus.data.gov.uk}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' UK <- LoadUK()
#' }
#' @seealso [LoadData()]
#' @export
LoadUK <- function() {
  # The COVID-19 data is from the UK API from Public Health England and NHSX: https://coronavirus.data.gov.uk
  geomUnitedKingdom <- pop_uk <- misc_uk <- NULL
  utils::data("geomUnitedKingdom", envir = environment())
  utils::data("pop_uk", envir = environment())
  utils::data("misc_uk", envir = environment())
  
  geomUnitedKingdom <- sf::st_as_sf(geomUnitedKingdom)

  cur_date <- lubridate::today()

  data_cur <- dataQueryUK(cur_date)
  past_date <- cur_date - 14
  data_past <- dataQueryUK(past_date)


  if (nrow(data_cur) < 180) { # likely does not include Scotland and Wales due to differences in update timing.
    cur_date_sep <- lubridate::floor_date(data_cur$date[1], "week") + 3 - 7 # try using data from the previous Wednesday
    data_cur_sep <- dataQueryUK(cur_date_sep)
    past_date_sep <- cur_date_sep - 14
    data_past_sep <- dataQueryUK(past_date_sep)

    CODES <- which(!(data_cur_sep$code %in% data_cur$code))
    if (length(CODES) > 0) { # if there are regions to add
      data_cur <- rbind(data_cur, data_cur_sep[CODES, ])
      CODES_past_o <- which(data_past$code %in% data_cur_sep$code[CODES])
      CODES_past_n <- which(data_past_sep$code %in% data_cur_sep$code[CODES])
      data_past <- rbind(data_past[-CODES_past_o, ], data_past_sep[CODES_past_n, ])
    }
  }

  # population
  # pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/uk_pop.csv", stringsAsFactors = FALSE) %>% select(-c("name"))
  pop <- pop_uk %>%
    tidyr::drop_na() %>%
    dplyr::select(-c("name"))

  # Join geom and pop of Hackney and City of London
  # Hackney: (E09000012) ; C.o.L. (E09000001)
  pop$pop[pop$code == "E09000012"] <- pop$pop[pop$code == "E09000012"] + pop$pop[pop$code == "E09000001"]

  # Join geom and pop of Cornwall and the Isles of Scilly
  # Cornwall (E06000052) ; Isles of Scilly (E06000053)
  pop$pop[pop$code == "E06000052"] <- pop$pop[pop$code == "E06000052"] + pop$pop[pop$code == "E06000053"]


  # geography
  # geom <- st_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson", stringsAsFactors = FALSE) %>%
  #  rename(code = ctyua19cd, name = ctyua19nm, welshname = ctyua19nmw)
  # Add Welsh language place names
  # for(bb in 195:216){
  # 	if (geom$name[bb]!=geom$welshname[bb]){
  # 		geom$name[bb] = paste0(geom$name[bb],"/",geom$welshname[bb])
  # 	}
  # }
  # Join geom and pop of Hackney and City of London
  # Hackney: (E09000012) ; C.o.L. (E09000001)
  # IND = which(geom$code=="E09000012")
  # HMM<-st_union(geom[c(IND,which(geom$code=="E09000001")),]) %>% st_cast("MULTIPOLYGON")
  # geom$geometry[IND] = HMM
  # geom$name[IND] = "Hackney and the City of London"
  # Join geom and pop of Cornwall and the Isles of Scilly
  # Cornwall (E06000052) ; Isles of Scilly (E06000053)
  # IND = which(geom$code=="E06000052")
  # HMM<-st_union(geom[c(IND,which(geom$code=="E06000053")),]) %>% st_cast("MULTIPOLYGON")
  # geom$geometry[IND] = HMM
  # geom$name[IND] = "Cornwall and the Isles of Scilly"
  # remove C.o.L and I.Scilly from geom
  # geom = geom[-c(which(geom$code=="E09000001"),which(geom$code=="E06000053")),]
  # st_write(geom,"countries/data/geom/geomUnitedKingdom.geojson")
  # geom <- st_read("countries/data/geom/geomUnitedKingdom.geojson")

  # misc <- vroom("countries/data/miscUK.csv")

  # integrate datasets

  data_join <- data_cur %>%
    dplyr::inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    dplyr::inner_join(pop, by = c("code")) %>%
    dplyr::inner_join(misc_uk, by = "code")

  data_join$Difference <- (data_join$cases - data_join$cases_past) * 10 / 14
  UKMap <- dplyr::inner_join(geomUnitedKingdom, data_join, by = c("micro_code" = "code")) # %>% # It's worth asking if we want to get rid of the "city of"'s, but if we do we can do this.
  # mutate(
  #   micro_name = str_replace(micro_name, ", City of$","")
  # )

  UKMap$RegionName <- paste(UKMap$micro_name, UKMap$country_name, sep = ", ")
  # If the Welsh names aren't already present, then:
  # sprintf(paste("%s",UKMap$country_name, sep=", "), if_else(is.na(UKMap$welshname)==F & UKMap$welshname != UKMap$micro_name, paste(UKMap$micro_name,UKMap$welshname, sep="/"), UKMap$micro_name))
  UKMap$Country <- UKMap$country_name
  UKMap$DateReport <- as.character(UKMap$date)
  UKMap$pInf <- UKMap$Difference / UKMap$pop
  UK_DATA <- subset(UKMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(UK_DATA)
}
