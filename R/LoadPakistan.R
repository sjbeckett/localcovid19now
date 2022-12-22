#' LoadPakistan
#'
#' @description Reads in subnational data for Pakistan to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data obtained from COVID-19 Data Hub \url{https://covid19datahub.io}
#'
#' @references
#' Guidotti et al., (2020). COVID-19 Data Hub. Journal of Open Source Software, 5(51), 2376, \url{https://doi.org/10.21105/joss.02376}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Pakistan <- LoadPakistan()
#' @seealso [LoadData()]
#' @export
LoadPakistan <- function() {
  # Data obtained from COVID-19 Data Hub https://covid19datahub.io

  geomPakistan <- NULL
  utils::data("geomPakistan", envir = environment())
  geomPakistan <- sf::st_as_sf(geomPakistan)

  # Guidotti et al., (2020). COVID-19 Data Hub. Journal of Open Source Software, 5(51), 2376, https://doi.org/10.21105/joss.02376

  x <- as.data.frame(COVID19::covid19("Pakistan", level = 2, verbose = FALSE))

  regions <- unique(x$administrative_area_level_2)
  DateReport <- c()
  CaseDifference <- c()
  pInf <- c()
  for (aa in 1:length(regions)) {
    subsetdata <- x[which(x$administrative_area_level_2 == regions[aa]), ]
    DateReport[aa] <- as.character(max(subsetdata$date))
    CaseDifference[aa] <- (10 / 14) * diff(range(utils::tail(subsetdata$confirmed, 14)))
    pInf[aa] <- CaseDifference[aa] / subsetdata$population[1]
  }

  caseTable <- data.frame(regions, DateReport, pInf)

  # geography


  # integrate datasets
  MapPakistan <- dplyr::inner_join(geomPakistan, caseTable, by = c("micro_name" = "regions"))
  # MapPakistan$geoid <- MapPakistan$shapeISO
  MapPakistan$RegionName <- paste(MapPakistan$micro_name, "Pakistan", sep = ", ")
  MapPakistan$Country <- "Pakistan"

  PAKISTAN_DATA <- subset(MapPakistan, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(PAKISTAN_DATA)
}
