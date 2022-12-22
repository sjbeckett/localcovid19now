#' LoadSouthAfrica
#'
#' @description Reads in subnational data for South Africa to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data Science for Social Impact Research Group @ University of Pretoria, Coronavirus COVID-19 (2019-nCoV) Data Repository for South Africa. Available on: \url{https://github.com/dsfsi/covid19za}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' SouthAfrica <- LoadSouthAfrica()
#' @seealso [LoadData()]
#' @export
LoadSouthAfrica <- function() {
  # Data Science for Social Impact Research Group @ University of Pretoria, Coronavirus COVID-19 (2019-nCoV) Data Repository for South Africa. Available on: https://github.com/dsfsi/covid19za

  # geometry, see: https://dataportal-mdb-sa.opendata.arcgis.com/datasets/23e1b3458c704f65bc764168ae8557b8_0/data?geometry=-17.204%2C-35.168%2C66.424%2C-21.692&selectedAttribute=PROVINCE
  # geomSouthAfrica$PROVINCE[geomSouthAfrica$PROVINCE=="GT"]="GP"
  # geomSouthAfrica$PROVINCE[geomSouthAfrica$PROVINCE=="LIM"]="LP"
  utils::data(list = c("geomSouthAfrica", "pop_southafrica"), envir = environment())
  geomSouthAfrica <- sf::st_as_sf(geomSouthAfrica)

  PRO <- unique(geomSouthAfrica$micro_code)

  # covid case data, see: https://github.com/dsfsi/covid19za
  COVID_data_SA <- vroom::vroom("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv", show_col_types = FALSE, progress = FALSE)

  DiffCases <- c()
  Date <- c()
  for (aa in 1:length(PRO)) {
    THIS <- dplyr::select(COVID_data_SA, PRO[aa])
    DiffCases[aa] <- diff(range(utils::tail(THIS, 14))) * 10 / 14
    Date[aa] <- as.character(utils::tail(as.Date(COVID_data_SA$date, format = "%d-%m-%Y"), 1))
  }
  geomSouthAfrica$DiffCases <- DiffCases
  geomSouthAfrica$date <- Date

  # population data, see: https://github.com/dsfsi/covid19za/blob/master/data/district_data/za_province_pop.csv
  # pop_southafrica <-read.csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/district_data/za_province_pop.csv",header = FALSE)
  # data("pop_southafrica")

  PROVINCECode <- c("GP", "KZN", "WC", "EC", "LP", "MP", "NW", "FS", "NC")
  pop_southafrica <- cbind(pop_southafrica, PROVINCECode)
  names(pop_southafrica) <- c("Name", "Population", "ProCode")

  # Link together
  SA_Map <- dplyr::inner_join(geomSouthAfrica, pop_southafrica, by = c("micro_code" = "ProCode"))
  SA_Map$RegionName <- paste(SA_Map$Name, SA_Map$country_name, sep = ", ") # This needs to be 'Name' because the geometry file doesn't have 'micro_name's
  SA_Map$Country <- SA_Map$country_name
  SA_Map$DateReport <- SA_Map$date
  SA_Map$pInf <- SA_Map$DiffCases / SA_Map$Population
  SA_DATA <- subset(SA_Map, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(SA_DATA)
}
