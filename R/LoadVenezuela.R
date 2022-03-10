#' Title
#'
#' @return
#' @export
#'
#' @examples
LoadVenezuela <- function() {
  # Aggregated from local resources by OCHA Venezuela:  https://data.humdata.org/dataset/corona-virus-covid-19-cases-and-deaths-in-venezuela

  casedata <- vroom("https://docs.google.com/spreadsheets/d/e/2PACX-1vQI4s0no2TS1dYxbv82nhKD7iz8fbDGwdsOI4kzJ0cg3gjOR51KIw_rNOff97Xic_fRQD41xmsDGUfM/pub?gid=1029482781&single=true&output=csv")

  regions <- names(casedata)[1:25]
  DateReport <- rep(tail(casedata$date, 1), length(regions))

  CaseDifference <- c()

  for (aa in 1:length(regions)) {
    eligiblerecordindex <- which(as.Date(casedata$date) > (as.Date(DateReport[1]) - 14))
    daysdiff <- as.numeric(diff(range(as.Date(casedata$date[eligiblerecordindex]))) + 1)
    eligible <- casedata[eligiblerecordindex, aa]
    CaseDifference[aa] <- 10 * diff(range(eligible)) / daysdiff
  }

  caseTable <- data.frame(regions, DateReport, CaseDifference)

  # population
  Vpop <- vroom("countries/data/VenezuelaPop.csv")
  VZdf <- inner_join(caseTable, Vpop, by = c("regions" = "State"))

  # geometry
  # geomVenezuela = st_read("https://github.com/deldersveld/topojson/raw/master/countries/venezuela/venezuela-estados.json")
  geomVenezuela <- st_read("countries/data/geom/geomVenezuela.geojson")
  geomVenezuela$micro_name[which(geomVenezuela$micro_name == "Vargas")] <- "La Guaira"

  # rename VZdf to match map
  VZdf$regions[which(VZdf$regions == "Delta.Amacuro")] <- "Delta Amacuro"
  VZdf$regions[which(VZdf$regions == "Distrito.Capital")] <- "Distrito Capital"
  VZdf$regions[which(VZdf$regions == "Nueva.Esparta")] <- "Nueva Esparta"
  VZdf$regions[which(VZdf$regions == "Los.Roques")] <- "Dependencias Federales"
  VZdf$regions[which(VZdf$regions == "La.Guaira")] <- "La Guaira"

  VenezuelaMap <- inner_join(geomVenezuela, VZdf, by = c("micro_name" = "regions"))
  VenezuelaMap$DateReport <- as.character(VenezuelaMap$DateReport)
  VenezuelaMap$RegionName <- paste(VenezuelaMap$micro_name, VenezuelaMap$country_name, sep = ", ")
  VenezuelaMap$pInf <- VenezuelaMap$CaseDifference / VenezuelaMap$Population_2011
  VenezuelaMap$Country <- VenezuelaMap$country_name

  VENEZUELA_DATA <- subset(VenezuelaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(VENEZUELA_DATA)
}
