#' LoadVenezuela
#'
#' @description Reads in subnational data for Venezuela to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data is aggregated from local resources by OCHA Venezuela:  \url{https://data.humdata.org/dataset/corona-virus-covid-19-cases-and-deaths-in-venezuela}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Venezuela <- LoadVenezuela()
#' @seealso [LoadData()]
#' @export
LoadVenezuela <- function() {
  # Aggregated from local resources by OCHA Venezuela:  https://data.humdata.org/dataset/corona-virus-covid-19-cases-and-deaths-in-venezuela

  pop_venezuela <- geomVenezuela <- NULL
  utils::data(list = c("geomVenezuela","pop_venezuela"), envir = environment())
  
  geomVenezuela <- sf::st_as_sf(geomVenezuela)

  casedata <- vroom::vroom("https://docs.google.com/spreadsheets/d/e/2PACX-1vQI4s0no2TS1dYxbv82nhKD7iz8fbDGwdsOI4kzJ0cg3gjOR51KIw_rNOff97Xic_fRQD41xmsDGUfM/pub?gid=1029482781&single=true&output=csv", show_col_types = FALSE, progress = FALSE)

  regions <- names(casedata)[1:25]
  DateReport <- rep(utils::tail(casedata$date, 1), length(regions))

  CaseDifference <- c()

  for (aa in 1:length(regions)) {
    eligiblerecordindex <- which(as.Date(casedata$date) > (as.Date(DateReport[1]) - 14))
    daysdiff <- as.numeric(diff(range(as.Date(casedata$date[eligiblerecordindex]))) + 1)
    eligible <- casedata[eligiblerecordindex, aa]
    CaseDifference[aa] <- 10 * diff(range(eligible)) / daysdiff
  }

  caseTable <- data.frame(regions, DateReport, CaseDifference)

  # population
  VZdf <- dplyr::inner_join(caseTable, pop_venezuela, by = c("regions" = "State"))

  # geometry
  # geomVenezuela = st_read("https://github.com/deldersveld/topojson/raw/master/countries/venezuela/venezuela-estados.json")
  # geomVenezuela <- st_read("countries/data/geom/geomVenezuela.geojson")
  geomVenezuela$micro_name[which(geomVenezuela$micro_name == "Vargas")] <- "La Guaira"

  # rename VZdf to match map
  VZdf$regions[which(VZdf$regions == "Delta.Amacuro")] <- "Delta Amacuro"
  VZdf$regions[which(VZdf$regions == "Distrito.Capital")] <- "Distrito Capital"
  VZdf$regions[which(VZdf$regions == "Nueva.Esparta")] <- "Nueva Esparta"
  VZdf$regions[which(VZdf$regions == "Los.Roques")] <- "Dependencias Federales"
  VZdf$regions[which(VZdf$regions == "La.Guaira")] <- "La Guaira"

  VenezuelaMap <- dplyr::inner_join(geomVenezuela, VZdf, by = c("micro_name" = "regions"))
  VenezuelaMap$DateReport <- as.character(VenezuelaMap$DateReport)
  VenezuelaMap$RegionName <- paste(VenezuelaMap$micro_name, VenezuelaMap$country_name, sep = ", ")
  VenezuelaMap$pInf <- VenezuelaMap$CaseDifference / VenezuelaMap$Population_2011
  VenezuelaMap$Country <- VenezuelaMap$country_name

  VENEZUELA_DATA <- subset(VenezuelaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(VENEZUELA_DATA)
}
