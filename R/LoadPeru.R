#' LoadPeru
#'
#' @description Reads in subnational data for Peru to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data obtained from COVID-19 Data Hub \url{https://covid19datahub.io} and
#' sourced from \url{https://github.com/jmcastagnetto/covid-19-peru-data}
#'
#' @references
#' Guidotti et al., (2020). COVID-19 Data Hub. Journal of Open Source Software, 5(51), 2376, \url{https://doi.org/10.21105/joss.02376}
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Peru <- LoadPeru()
#' @seealso [LoadData()]
#' @export
LoadPeru <- function() {
  # Data obtained from COVID-19 Data Hub https://covid19datahub.io
  # sourced from https://github.com/jmcastagnetto/covid-19-peru-data

  # Guidotti et al., (2020). COVID-19 Data Hub. Journal of Open Source Software, 5(51), 2376, https://doi.org/10.21105/joss.02376

  geomPeru <- NULL
  utils::data("geomPeru", envir = environment())
  geomPeru <- sf::st_as_sf(geomPeru)

  x <- as.data.frame(COVID19::covid19("Peru", level = 2, verbose = FALSE))

  regions <- unique(x$administrative_area_level_2)
  DateReport <- c()
  CaseDifference <- c()
  pInf <- c()
  for (aa in 1:length(regions)) {
    subsetdata <- x[which(x$administrative_area_level_2 == regions[aa]), ]
    DateReport[aa] <- as.character(max(subsetdata$date))
    # CaseDifference[aa] = (10/14)*diff(range(tail(subsetdata$confirmed,14)))
    ## diff(range(tail(subsetdata$confirmed,14))) returns NA
    currentInd <- nrow(subsetdata) - 1 # most recent date may not have case updates.
    CaseDifference[aa] <- (10 / 14) * (subsetdata$confirmed[currentInd] - subsetdata$confirmed[currentInd - 14])
    pInf[aa] <- CaseDifference[aa] / subsetdata$population[1]
  }

  caseTable <- data.frame(regions, DateReport, pInf)
  # caseTable$regionUpper = toupper(regions)
  # geography
  # geomPeru = st_read("https://github.com/juaneladio/peru-geojson/raw/master/peru_departamental_simple.geojson")
  # data("geomPeru")

  # integrate datasets
  MapPeru <- dplyr::inner_join(geomPeru, caseTable, by = c("micro_name" = "regions"))

  MapPeru$RegionName <- paste(MapPeru$micro_name, MapPeru$country_name, sep = ", ")
  MapPeru$Country <- MapPeru$country_name

  PERU_DATA <- subset(MapPeru, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(PERU_DATA)
}
