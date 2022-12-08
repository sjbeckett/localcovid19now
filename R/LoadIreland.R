#' LoadIreland
#'
#' @description Reads in subnational data for Ireland to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data is provided by the Health Service Executive (HSE), Health Protection Surveillance Centre (HPSC), The Central Statistics Office (CSO) and Gov.ie and accessed via Ireland's COVID-19 Data Hub: \url{https://covid19ireland-geohive.hub.arcgis.com/}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Ireland <- LoadIreland()
#' @seealso [LoadData()]
#' @export
LoadIreland <- function() {
  TimeStamp <- CountyName <- ConfirmedCovidCases <- PopulationCensus16 <- cases <- pop <- miscIreland <- NULL
  # load in geometry and other data
  utils::data(list = c("geomIreland", "misc_ireland"), envir = environment())
  geomIreland <- sf::st_as_sf(geomIreland)
  # set codes in double to avoid "2e+05" vs 250000 errors
  geomIreland$micro_code <- as.double(geomIreland$micro_code)
  misc_ireland$CO_ID <- as.double(misc_ireland$CO_ID)

  # geom <<- st_read('https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/Ireland_Counties.geojson')
  # data("geomIreland")
  # Main COVID-19 hub page: https://covid-19.geohive.ie/datasets/d9be85b30d7748b5b7c09450b8aede63_0
  data <- vroom::vroom("https://opendata.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv", show_col_types = FALSE, progress = FALSE) %>%
    dplyr::mutate(date = as.Date(TimeStamp)) %>%
    dplyr::select(CountyName, date, cases = ConfirmedCovidCases, pop = PopulationCensus16) %>%
    dplyr::arrange(dplyr::desc(date))
  data_cur <- data %>%
    dplyr::group_by(CountyName) %>%
    dplyr::summarise(CountyName = dplyr::first(CountyName), cases = dplyr::first(cases), date = dplyr::first(date), pop = dplyr::first(pop)) %>%
    as.data.frame()
  past_date <- data_cur$date[1] - 14
  data_past <- data %>%
    dplyr::filter(date == past_date) %>%
    dplyr::group_by(CountyName) %>%
    dplyr::summarise(CountyName = dplyr::first(CountyName), cases = dplyr::first(cases), date = dplyr::first(date)) %>%
    as.data.frame()
  data_join <- dplyr::inner_join(data_cur, data_past, by = "CountyName", suffix = c("", "_past"))
  data_join$Difference <- (data_join$cases - data_join$cases_past) * 10 / 14
  #   miscIreland <- vroom::vroom("countries/data/miscIreland.csv", col_types = vroom::cols(CO_ID = vroom::col_character()))

  # integrate datasets
  IrelandMap <- dplyr::inner_join(geomIreland, data_join, by = c("micro_name" = "CountyName")) %>%
    dplyr::inner_join(misc_ireland, by = c("micro_code" = "CO_ID"))
  IrelandMap$RegionName <- paste(paste(IrelandMap$micro_name, IrelandMap$CONTAE, sep = "/"), IrelandMap$country_name, sep = ", ")
  IrelandMap$Country <- IrelandMap$country_name
  IrelandMap$DateReport <- as.character(IrelandMap$date)
  IrelandMap$pInf <- IrelandMap$Difference / IrelandMap$pop

  IRELAND_DATA <- subset(IrelandMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(IRELAND_DATA)
}
