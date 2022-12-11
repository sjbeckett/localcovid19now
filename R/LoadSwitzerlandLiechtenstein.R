#' LoadSwitzerlandLiechtenstein
#'
#' @description Reads in subnational data for Switzerland and also data for Liechtenstein to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data obtained via the Federal Office of Public Health FOPH \url{https://www.covid19.admin.ch/en/overview}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' SwitzerlandLiechtenstein <- LoadSwitzerlandLiechtenstein()
#' }
#' @seealso [LoadData()]
#' @export
LoadSwitzerlandLiechtenstein <- function() {
  # Federal Office of Public Health FOPH https://www.covid19.admin.ch/en/overview

  geomSwitzerlandLiechtenstein <- NULL
  utils::data("geomSwitzerlandLiechtenstein", envir = environment())
  geomSwitzerlandLiechtenstein <- sf::st_as_sf(geomSwitzerlandLiechtenstein)

  # 1. import API to find code for most recent file version (date and code change for new data)
  datastructure <- jsonlite::fromJSON("https://www.covid19.admin.ch/api/data/context")
  # 2. find URL for case data by region and read in
  chURL <- datastructure$sources$individual$csv$daily$cases
  CHdata <- vroom::vroom(chURL, show_col_types = FALSE, progress = FALSE)

  # 3. only need regional data, not that for whole country (CH) or whole dataset (CHFL)
  CHdata <- CHdata[-c(which(CHdata$geoRegion == "CH"), which(CHdata$geoRegion == "CHFL")), ]
  code <- unique(CHdata$geoRegion)

  # 4. for each region want $sum14d from the second to most recent entry (as most recent may be partial data), and average to 10 day cases per population
  DateReport <- c()
  pInf <- c()
  for (aa in 1:length(code)) {
    subsetdata <- CHdata[which(CHdata$geoRegion == code[aa]), ]
    LL <- nrow(subsetdata) - 1
    pInf[aa] <- 10 / 14 * (subsetdata$sumTotal[LL] - subsetdata$sumTotal[LL - 14]) / subsetdata$pop[LL]
    DateReport[aa] <- as.character(subsetdata$datum[LL])
  }
  data_join <- data.frame(code, DateReport, pInf)

  # 5. load geometry
  # geometry
  # geom <<- st_read("https://gist.githubusercontent.com/mbostock/4207744/raw/3232c7558742bab53227e242a437f64ae4c58d9e/readme-swiss.json")
  # geom <- st_set_crs(geom, 4326)
  # geom <- st_read("countries/data/geom/geomSwitzerland.geojson")
  # geom <- sf::st_read("countries/data/geom/geomSwitzerlandLiechtenstein.geojson")

  # 6. integrate datasets
  SwitzerlandMap <- dplyr::inner_join(geomSwitzerlandLiechtenstein, data_join, by = c("micro_code" = "code"))
  SwitzerlandMap$RegionName <- paste(SwitzerlandMap$micro_name, SwitzerlandMap$country_name, sep = ", ")
  SwitzerlandMap$Country <- SwitzerlandMap$country_name

  SWITZERLAND_DATA <- subset(SwitzerlandMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(SWITZERLAND_DATA)
}
