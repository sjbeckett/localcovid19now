#' LoadAustralia
#'
#' @description Reads in subnational data for Australia to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data was obtained from \url{https://github.com/M3IT/COVID-19_Data} and aggregated by www.covid19data.com.au from local health resources.
#'
#' @examples
#' Australia <- LoadAustralia()
#' @seealso [LoadData()]
#' @export
LoadAustralia <- function() {
  # Load in geomtry and population data
  pop_australia <- NULL
  utils::data("geomAustralia", envir = environment())
  utils::data("pop_australia", envir = environment())
  geomAustralia <- sf::st_as_sf(geomAustralia)

  # COVID-19 data was obtained from https://github.com/M3IT/COVID-19_Data and aggregated by www.covid19data.com.au from local health resources.
  data <- vroom::vroom("https://raw.githubusercontent.com/M3IT/COVID-19_Data/master/Data/COVID_AU_state_cumulative.csv", show_col_types = FALSE, progress = FALSE)

  data$date <- as.Date(data$date)
  data <- data[rev(order(data$date)), c("date", "state", "confirmed")]
  stateList <- unique(data$state)
  today <- data$date[1]
  past <- today - 14
  australiaCases <- data.frame(Date = as.character(), state = as.character(), Difference = as.numeric())
  for (i in 1:length(stateList)) {
    difference <- (data[(data$date == today & data$state == stateList[i]), "confirmed"] - data[(data$date == past & data$state == stateList[i]), "confirmed"]) * 10 / 14
    vec <- data.frame(Date = today, state = stateList[i], Difference = difference)
    australiaCases <- rbind(australiaCases, vec)
  }

  australiadf <- dplyr::inner_join(australiaCases, pop_australia, by = "state")

  AustraliaMap <- dplyr::inner_join(geomAustralia, australiadf, by = c("micro_name" = "state"))
  AustraliaMap$RegionName <- paste(AustraliaMap$micro_name, AustraliaMap$country_name, sep = ", ")
  AustraliaMap$Country <- AustraliaMap$country_name
  AustraliaMap$DateReport <- as.character(AustraliaMap$Date)
  AustraliaMap$pInf <- AustraliaMap$confirmed / AustraliaMap$Population
  Australia_DATA <- subset(AustraliaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(Australia_DATA)
}
