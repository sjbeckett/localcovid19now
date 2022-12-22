#' LoadSpain
#'
#' @description Reads in subnational data for Spain to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' COVID-19 data from España Ministerio de Sanidad and Instituto de Salud Carlos III: \url{https://cnecovid.isciii.es/covid19/}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Spain <- LoadSpain()
#' @seealso [LoadData()]
#' @export
LoadSpain <- function() {
  # COVID-19 data from España Ministerio de Sanidad and Instituto de Salud Carlos III: https://cnecovid.isciii.es/covid19/

  # county geojson data
  # geomSpain <- st_read('https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/spain-provinces.geojson')
  # data("geomSpain")
  # Main COVID-19 hub page: https://cnecovid.isciii.es/covid19/#distribuci%C3%B3n-geogr%C3%A1fica

  geomSpain <- misc_spain <- NULL
  utils::data(list = c("geomSpain", "misc_spain"), envir = environment())
  geomSpain <- sf::st_as_sf(geomSpain)
  
  SPAIN <- utils::read.csv("https://cnecovid.isciii.es/covid19/resources/casos_tecnica_provincia.csv", na.strings = FALSE)

  # code link file
  # data("misc_spain")
  # Population data comes from  Instituto Nacional de Estadística: https://www.ine.es/jaxiT3/Datos.htm?t=2852#!tabs-tabla

  DataJoin <- c()
  Counties <- unique(SPAIN$provincia_iso)
  DataJoin$ProvinceName <- Counties
  for (aa in 1:length(Counties)) {
    Subset <- SPAIN[SPAIN$provincia_iso == Counties[aa], ]
    Dates <- as.Date(Subset$fecha)
    LL <- length(Dates)
    ConfirmedCovidCases <- cumsum(Subset$num_casos)
    CaseDiff <- (ConfirmedCovidCases[LL] - ConfirmedCovidCases[LL - 14]) / 14 * 10
    # Make sure difference in cases is positive. If not set to NA.
    if (CaseDiff < 0) {
      CaseDiff <- NA
    }
    DataJoin$LatestTime[aa] <- as.character(Dates[LL])
    DataJoin$CaseDiff[aa] <- CaseDiff
  }


  # integrate datasets
  Spaindata <- as.data.frame(DataJoin)
  Spaindf <- dplyr::inner_join(Spaindata, misc_spain, by = c("ProvinceName" = "code"))
  SpainMap <- dplyr::inner_join(geomSpain, Spaindf, by = c("micro_name" = "name"))

  SpainMap$RegionName <- paste(SpainMap$micro_name, SpainMap$country_name, sep = ", ")
  SpainMap$Country <- SpainMap$country_name
  SpainMap$DateReport <- as.character(SpainMap$LatestTime)
  SpainMap$pInf <- SpainMap$CaseDiff / SpainMap$population2019
  SPAIN_DATA <- subset(SpainMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(SPAIN_DATA)
}
