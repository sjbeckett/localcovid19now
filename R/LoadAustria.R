#' LoadAustria
#'
#' @description Reads in subnational data for Austria to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data is from the Federal Ministry for Social Affairs, Health, Care and Consumer Protection (BMSGPK) data on COVID-19 for Austria \url{https://www.data.gv.at/covid-19/}.
#'
#' @examples
#' \dontrun{
#' Austria <- LoadAustria()
#' }
#' @seealso [LoadData]
#' @export
LoadAustria <- function() {
  # Load in geometry
  utils::data("geomAustria", envir = environment())
  geomAustria <- sf::st_as_sf(geomAustria)

  Time <- Bezirk <- GKZ <- AnzEinwohner <- AnzahlFaelleSum <- code <- cases <- population <- NULL

  # Federal Ministry for Social Affairs, Health, Care and Consumer Protection (BMSGPK) data on COVID-19 for Austria: https://www.data.gv.at/covid-19/
  # case data
  data <- vroom::vroom("https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline_GKZ.csv", delim = ";", show_col_types = FALSE, progress = FALSE) %>%
    dplyr::select(date = Time, name = Bezirk, code = GKZ, population = AnzEinwohner, cases = AnzahlFaelleSum)
  # date is in format dd.mm.YYYY HH:MM:SS
  # reformat the date
  for (i in 1:length(data$date)) {
    data$date[i] <- unlist(strsplit(data$date[i], " "))[1]
  }
  data <- data %>%
    dplyr::mutate(
      date = as.Date(format(strptime(as.character(date), "%d.%m.%Y"), "%Y-%m-%d")),
      code = as.character(code)
    ) %>%
    dplyr::arrange(dplyr::desc(date)) %>%
    dplyr::filter(!is.na(cases))

  # find case difference
  cur_date <- max(as.Date(data$date))
  past_date <- lubridate::ymd(cur_date) - 14
  data_cur <- data %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(code = dplyr::first(code), cases = dplyr::first(cases), date = dplyr::first(date), pop = dplyr::first(population)) %>%
    as.data.frame()
  data_past <- data %>%
    dplyr::filter(date <= past_date) %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(code = dplyr::first(code), cases = dplyr::first(cases), date = dplyr::first(date)) %>%
    as.data.frame()
  data_join <- data_cur %>%
    dplyr::inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    dplyr::mutate(n = date - past_date)
  data_join$Difference <- (data_join$cases - data_join$cases_past) * 10 / 14

  # geometry
  # geomAustria <- st_read('https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2017/simplified-99.9/bezirke_999_geo.json')

  # integrate datsets
  AustriaMap <- dplyr::inner_join(geomAustria, data_join, by = c("micro_code" = "code"))
  AustriaMap$RegionName <- paste(AustriaMap$micro_name, AustriaMap$country_name, sep = ", ")
  AustriaMap$Country <- AustriaMap$country_name
  AustriaMap$DateReport <- as.character(AustriaMap$date)
  AustriaMap$pInf <- AustriaMap$Difference / AustriaMap$pop
  AUSTRIA_DATA <- subset(AustriaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(AUSTRIA_DATA)
}
