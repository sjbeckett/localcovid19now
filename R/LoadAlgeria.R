#' LoadAlgeria
#'
#' @description Reads in subnational data for Algeria to calculate most recent estimate of per capita active COVID-19 cases.
#'
#' @note
#' Data was obtained using the Algeria Coronavirus Tracker API \url{https://corona-dz.live/}, documented at \url{https://github.com/Amine27/covid-19-dz} whcih sources information from the Algerian Ministry of Health, Population and Hospital Reform website.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Algeria <- LoadAlgeria()
#' }
#' @seealso [LoadCountries()]
#' @export
LoadAlgeria <- function() {
  pop_algeria <- NULL
  
  # Algeria Coronavirus Tracker API https://corona-dz.live/ https://github.com/Amine27/covid-19-dz
  ALG <- jsonlite::fromJSON("https://api.corona-dz.live/province/all")

  #Load in geometry and population data.
  utils::data("geomAlgeria", envir = environment())
  utils::data("pop_algeria", envir = environment())
  geomAlgeria <- sf::st_as_sf(geomAlgeria)
  

  # geography
  # https://github.com/Amine27/covid-19-dz/blob/master/static/map/algeria.json
  # geomAlgeria = st_read("https://github.com/Amine27/covid-19-dz/raw/master/static/map/algeria.json")
  # geomAlgeria

  # Change names in data set (ALG) to match names in geometry

  ## Avoid encoding issues with characters/diacriticals
  name2save1 <- geomAlgeria$micro_name[15] # "Sidi Bel Abbès"
  name2save2 <- geomAlgeria$micro_name[39] # "Aïn Defla"
  name2save3 <- geomAlgeria$micro_name[41] # "Aïn Témouchent"

  # regions
  ## Add diacritical marks lost in json
  provinces <- ALG$name
  provinces[provinces == "Ain Defla"] <- name2save2
  provinces[provinces == "Ain Temouchent"] <- name2save3
  provinces[provinces == "Algiers"] <- "Alger"
  provinces[provinces == "Oum El Bouaghi"] <- "Oum el Bouaghi"
  provinces[provinces == "Sidi Bel Abbes"] <- name2save1
  provinces[provinces == "Tamanrasset"] <- "Tamanghasset"

  DateReport <- c()
  CaseDifference <- c()
  for (aa in 1:length(provinces)) {
    subsetdata <- ALG$data[[aa]]
    DateReport[aa] <- as.character(max(as.Date(subsetdata$date)))
    CaseDifference[aa] <- sum(subsetdata$newConfirmed[as.Date(subsetdata$date) > max(as.Date(subsetdata$date)) - 14]) * (10 / 14)
  }

  caseTable <- data.frame(provinces, DateReport, CaseDifference)

  # population
  # pop_algeria # 2008 census

  Algeriadf <- dplyr::inner_join(caseTable, pop_algeria, by = c("provinces" = "Name"))

  AlgeriaMap <- dplyr::inner_join(geomAlgeria, Algeriadf, by = c("micro_name" = "provinces"))
  AlgeriaMap$pInf <- AlgeriaMap$CaseDifference / AlgeriaMap$population
  AlgeriaMap$RegionName <- paste(AlgeriaMap$micro_name, AlgeriaMap$country_name, sep = ", ")
  AlgeriaMap$Country <- AlgeriaMap$country_name
  ALGERIA_DATA <- subset(AlgeriaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(ALGERIA_DATA)
}
