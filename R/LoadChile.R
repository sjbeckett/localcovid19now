#' LoadChile
#'
#' @description Reads in subnational data for Chile to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data obtained from COVID-19 Data Hub \url{https://covid19datahub.io}
#' sourced from Ministerio de Ciencia, Tecnología, Conocimiento e Innovación: \url{https://www.minciencia.gob.cl/COVID19/}.
#'
#' @references
#' Guidotti et al., (2020). COVID-19 Data Hub. Journal of Open Source Software, 5(51), 2376, \url{https://doi.org/10.21105/joss.02376}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Chile <- LoadChile()
#' }
#' @seealso [LoadData()]
#' @export
LoadChile <- function() {
  # Data obtained from COVID-19 Data Hub https://covid19datahub.io
  # sourced from Ministerio de Ciencia, Tecnología, Conocimiento e Innovación: https://www.minciencia.gob.cl/COVID19/.

  geomChile <- NULL

  utils::data("geomChile", envir = environment())
  geomChile <- sf::st_as_sf(geomChile)

  # Guidotti et al., (2020). COVID-19 Data Hub. Journal of Open Source Software, 5(51), 2376, https://doi.org/10.21105/joss.02376

  x <- as.data.frame(COVID19::covid19("Chile", level = 2, verbose = FALSE))

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
  # geomChile = st_read("https://github.com/caracena/chile-geojson/raw/master/regiones.json")
  # geomChile$matchName = c("Arica y Parinacota","Tarapacá","Antofagasta","Magallanes" ,"Aysén","Atacama", "Coquimbo","Valparaíso","Metropolitana","Los Lagos","Los Ríos","Araucania","Biobío","Ñuble","Maule","O'Higgins")

  # integrate datasets
  MapChile <- dplyr::inner_join(geomChile, caseTable, by = c("micro_name" = "regions"))

  MapChile$RegionName <- paste(MapChile$micro_name, MapChile$country_name, sep = ", ")
  MapChile$Country <- MapChile$country_name

  CHILE_DATA <- subset(MapChile, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(CHILE_DATA)
}
