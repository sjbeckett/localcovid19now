#' LoadMalaysia
#'
#' @description Reads in subnational data for Malaysia to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Official data on the COVID-19 epidemic in Malaysia. Powered by CPRC, CPRC Hospital System, MKAK, and MySejahtera.  \url{https://github.com/MoH-Malaysia/covid19-public}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Malaysia <- LoadMalaysia()
#' @seealso [LoadData()]
#' @export
LoadMalaysia <- function() {
  # Official data on the COVID-19 epidemic in Malaysia. Powered by CPRC, CPRC Hospital System, MKAK, and MySejahtera.  https://github.com/MoH-Malaysia/covid19-public

  pop_malaysia <- geomMalaysia <- NULL
  utils::data("geomMalaysia", envir = environment())
  utils::data("pop_malaysia", envir = environment())
  geomMalaysia <- sf::st_as_sf(geomMalaysia)

  casesbystate <- vroom::vroom("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_state.csv", show_col_types = FALSE, progress = FALSE) # new cases by state by time
  casesbystate$date <- lubridate::as_date(casesbystate$date)
  states <- unique(casesbystate$state)


  # population
  # pop  = read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/static/population.csv")
  # pop = vroom("countries/data/MalaysiaPop.csv") %>% select(state,pop)
  # data("pop_malaysia")
  DateReport <- c()
  pInf <- c()
  for (aa in 1:length(states)) {
    subset <- casesbystate[casesbystate$state == states[aa], ]
    DateReport[aa] <- as.character(max(subset$date))
    CaseDifference <- sum(subset$cases_new[which(subset$date > max(subset$date) - 14)]) / 14 * 10
    pInf[aa] <- CaseDifference / pop_malaysia$pop[pop_malaysia$state == states[aa]]
  }
  dataTable <- data.frame(state = states, DateReport, pInf)

  # geometry
  # geomMalaysia = st_read("https://raw.githubusercontent.com/jnewbery/MapMalaysia/master/public/data/states.geojson")
  # edit names to match with case data
  # geomMalaysia$Name[1] = "W.P. Kuala Lumpur"
  # geomMalaysia$Name[2] = "W.P. Labuan"
  # geomMalaysia$Name[3] = "W.P. Putrajaya"
  # geomMalaysia$Name[10] = "Pulau Pinang"
  # data("geomMalaysia")


  # integrate datasets
  MalaysiaMap <- dplyr::inner_join(geomMalaysia, dataTable, by = c("micro_name" = "state"))
  MalaysiaMap$Country <- MalaysiaMap$country_name

  MalaysiaMap$RegionName <- paste(MalaysiaMap$micro_name, MalaysiaMap$Country, sep = ", ")

  MALAYSIA <- subset(MalaysiaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(MALAYSIA)
}
