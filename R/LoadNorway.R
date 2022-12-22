#' LoadNorway
#'
#' @description Reads in subnational data for Norway to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Thomas, Haarstad, F., Manuel & YBK. Public COVID-19 Data for Norway (covid19data.no). \url{https://github.com/thohan88/covid19-nor-data}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Norway <- LoadNorway()
#' @seealso [LoadData()]
#' @export
LoadNorway <- function() {
  geomNorway <- NULL
  utils::data("geomNorway", envir = environment())
  geomNorway <- sf::st_as_sf(geomNorway)

  data <- vroom::vroom("https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality.csv", show_col_types = FALSE, progress = FALSE)
  data$date <- as.Date(data$date)

  ## THIS SCRIPT ASSUMES ALL COUNTIES UPDATE AT ONCE.
  today <- data$date[length(data$date)]
  latest_data <- data %>% dplyr::filter(data$date == today)
  latest_data <- latest_data[, c("date", "kommune_no", "kommune_name", "population", "cases")]
  past_data <- data %>% dplyr::filter(data$date == (today - 14))
  past_data <- past_data[, c("kommune_name", "cases")]
  norwaydf <- dplyr::inner_join(latest_data, past_data, by = "kommune_name")
  norwaydf$Difference <- (norwaydf$cases.x - norwaydf$cases.y) * 10 / 14
  norwaydf <- norwaydf[, c("date", "kommune_no", "kommune_name", "population", "Difference")]

  ### Geojson:
  # geomNorway <- st_read('https://github.com/smistad/konverter-norgeskart-projeksjon/releases/download/v2020/kommuner_komprimert.json')
  # geomNorway$kommunenummer <- as.numeric(geomNorway$kommunenummer)
  # geomNorway <- geomNorway[,c('kommunenummer','geometry')]
  # data("geomNorway")
  # integrate datasets
  norwayMap <- dplyr::inner_join(geomNorway, norwaydf, by = c("micro_code" = "kommune_no"))

  norwayMap$DateReport <- as.character(norwayMap$date)
  norwayMap$RegionName <- paste(norwayMap$micro_name, norwayMap$country_name, sep = ", ")
  norwayMap$Country <- norwayMap$country_name
  norwayMap$pInf <- norwayMap$Difference / norwayMap$population

  NORWAY_DATA <- subset(norwayMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(NORWAY_DATA)
}
