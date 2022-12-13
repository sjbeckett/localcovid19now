#' LoadBrazil
#'
#' @description Reads in subnational data for Brazil to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' COVID-19 data are aggregated from Ministério da Saúde and Brasil.IO by \url{https://github.com/wcota/covid19br}
#'
#' @references
#' W. Cota, “Monitoring the number of COVID-19 cases and deaths in brazil at municipal and federative units level”, SciELOPreprints:362 (2020), 10.1590/scielopreprints.362
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Brazil <- LoadBrazil()
#' }
#' @seealso [LoadData()]
#' @export
LoadBrazil <- function() {
  state <- pop2020 <- NULL
  # Load geometry
  utils::data("geomBrazil", envir = environment())
  geomBrazil <- sf::st_as_sf(geomBrazil)

  # COVID-19 data are aggregated from Ministério da Saúde and Brasil.IO by https://github.com/wcota/covid19br
  # W. Cota, “Monitoring the number of COVID-19 cases and deaths in brazil at municipal and federative units level”, SciELOPreprints:362 (2020), 10.1590/scielopreprints.362

  data <- vroom::vroom("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv", show_col_types = FALSE, progress = FALSE)
  data <- data[rev(order(lubridate::as_date(data$date))), c("date", "state", "totalCasesMS")]
  stateList <- unique(data$state)
  data$date <- lubridate::as_date(data$date)
  today <- data$date[1]
  past <- today - 14
  brazilCases <- data.frame(Date = as.character(), state = as.character(), Difference = as.numeric())
  for (i in 1:length(stateList)) {
    difference <- dplyr::pull((data[(data$date == today & data$state == stateList[i]), "totalCasesMS"] - data[(data$date == past & data$state == stateList[i]), "totalCasesMS"]) * 10 / 14)
    vec <- data.frame(Date = as.character(today), state = stateList[i], Difference = difference)
    brazilCases <- rbind(brazilCases, vec)
  }
  # data[(data$date == today & data$state == stateList[i]),'totalCasesMS']
  ## population
  pop <- vroom::vroom("https://raw.githubusercontent.com/wcota/covid19br/master/cities_info.csv", show_col_types = FALSE, progress = FALSE)
  pop <- pop[, c("state", "pop2020")]
  popState <- as.data.frame(pop %>% dplyr::group_by(state) %>% dplyr::summarise(pop2020 = sum(pop2020)))
  names(popState) <- c("state", "Population")
  brazildf <- dplyr::inner_join(brazilCases, popState, by = c("state"))

  ## geojson
  # geomBrazil <- st_read('https://raw.githubusercontent.com/marcioibm/brazil-states/master/br_states.geojson')

  BrazilMap <- dplyr::inner_join(geomBrazil, brazildf, by = c("micro_code" = "state"))
  BrazilMap$RegionName <- paste(BrazilMap$micro_name, BrazilMap$country_name, sep = ", ")
  BrazilMap$Country <- BrazilMap$country_name
  BrazilMap$DateReport <- as.character(BrazilMap$Date)
  BrazilMap$pInf <- BrazilMap$Difference / BrazilMap$Population
  Brazil_DATA <- subset(BrazilMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(Brazil_DATA)
}
