#' LoadSaudiArabia
#'
#' @description Reads in subnational data for Saudi Arabia to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data sourced from Ministry of Health, Covid19 Command and Control Center CCC, The National Health Emergency Operation Center NHEOC; and assembled by National Health Command and Control NHCC, Covid19 Data and Informatics Committee. \url{https://covid19.moh.gov.sa/}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' SaudiArabia <- LoadSaudiArabia()
#' }
#' @seealso [LoadData()]
#' @export
LoadSaudiArabia <- function() {
  # Data sourced from Ministry of Health, Covid19 Command and Control Center CCC, The National Health Emergency Operation Center NHEOC; and assembled by National Health Command and Control NHCC, Covid19 Data and Informatics Committee. https://covid19.moh.gov.sa/

  misc_saudiarabia <- pop_saudiarabia <- geomSaudiArabia <- NULL
  utils::data(list = c("misc_saudiarabia", "pop_saudiarabia", "geomSaudiArabia"), envir = environment())
  
  geomSaudiArabia <- sf::st_as_sf(geomSaudiArabia)

  Casesdataset <- sf::st_read("https://services6.arcgis.com/bKYAIlQgwHslVRaK/arcgis/rest/services/VWPlacesCasesHostedView/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json", quiet = TRUE)
  # note need to convert dates from UNIX timestamp

  # 13 regions, 146 governates.

  Regions <- unique(Casesdataset$RegionName_EN)
  DateReport <- c()
  CaseDifference <- c()
  RegionName <- c()
  for (aa in 1:length(Regions)) {
    subsetdata <- Casesdataset[which(Casesdataset$RegionName_EN == Regions[aa]), ]
    DATES <- as.Date(as.POSIXct(as.numeric(subsetdata$Reportdt) / 1000, origin = "1970-01-01")) # convert from UNIX timestamp
    LastDate <- max(DATES)
    DateReport[aa] <- as.character(LastDate)
    CaseDifference[aa] <- (10 / 14) * sum(subsetdata$Confirmed[which(DATES > (LastDate - 14))])
    RegionName[aa] <- paste(subsetdata$RegionName_AR[1], subsetdata$RegionName_EN[1], sep = "/")
  }
  caseTable <- data.frame(Regions, DateReport, CaseDifference, RegionName)

  # pop
  # data("pop_saudiarabia")
  # data("misc_saudiarabia")
  SAdf <- dplyr::inner_join(caseTable, pop_saudiarabia, by = c("Regions" = "Emirate")) %>%
    dplyr::inner_join(misc_saudiarabia, by = c("Regions" = "region_name_en"))

  # geom
  # geomSaudiArabia = st_read("https://services6.arcgis.com/bKYAIlQgwHslVRaK/arcgis/rest/services/CasesByRegion_ViewLayer/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  # data("geomSaudiArabia")

  # integrate datasets
  MapSaudiArabia <- dplyr::inner_join(geomSaudiArabia, SAdf, by = c("micro_name" = "Regions"))
  MapSaudiArabia$RegionName <- paste(MapSaudiArabia$RegionName, MapSaudiArabia$country_name, sep = ", ")
  MapSaudiArabia$Country <- MapSaudiArabia$country_name
  MapSaudiArabia$pInf <- MapSaudiArabia$CaseDifference / MapSaudiArabia$Population
  SAUDI_ARABIA_DATA <- subset(MapSaudiArabia, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(SAUDI_ARABIA_DATA)
}
