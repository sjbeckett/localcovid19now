#' LoadItaly
#'
#' @description Reads in subnational data for Italy to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data collated by the Italian Department of Civil Protection COVID-19: \url{https://github.com/pcm-dpc/COVID-19/}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Italy <- LoadItaly()
#' @seealso [LoadData()]
#' @export
LoadItaly <- function() {
  code <- cases <- region <- province <- pop_italy <- totale_casi <- codice_provincia <- denominazione_regione <- denominazione_provincia <- NULL

  # Load geometry and population data
  utils::data("geomItaly", envir = environment())
  utils::data("pop_italy", envir = environment())
  geomItaly <- sf::st_as_sf(geomItaly)

  # Italian Department of Civil Protection COVID-19 Data: https://github.com/pcm-dpc/COVID-19/
  # italy: need to download data_cur and data_past respectively

  latestdata <- vroom::vroom(paste0("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-latest.csv"), col_types = vroom::cols(note = vroom::col_character()), show_col_types = FALSE, progress = FALSE ) %>%
    dplyr::select(latest_date = data, region = denominazione_regione, province = denominazione_provincia, code = codice_provincia, cases = totale_casi)

  pastdate <- lubridate::date(latestdata$latest_date[1]) - lubridate::days(14)


  pastdata <- vroom::vroom(paste0("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-", stringr::str_replace_all(as.character(pastdate), "-", ""), ".csv"), col_types = vroom::cols(note = vroom::col_character()), show_col_types = FALSE, progress = FALSE) %>%
    dplyr::select(date = data, region = denominazione_regione, province = denominazione_provincia, code = codice_provincia, cases_past = totale_casi)


  data_join <- latestdata %>%
    dplyr::inner_join(pastdata, by = c("code")) %>%
    dplyr::inner_join(pop_italy, by = c("code"))
  data_join$CaseDiff <- (data_join$cases - data_join$cases_past) * 10 / 14
  data_join$date <- lubridate::as_date(data_join$latest_date)

  # geometry
  # geom <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/italy_simpler.geojson")
  # data("geomItaly")
  # integrate datasets

  ItalyMap <- dplyr::inner_join(geomItaly, data_join, by = c("micro_code" = "code"))
  ItalyMap$RegionName <- paste(ItalyMap$name, ItalyMap$country_name, sep = ", ")
  ItalyMap$Country <- ItalyMap$country_name
  ItalyMap$DateReport <- as.character(ItalyMap$date)
  ItalyMap$pInf <- ItalyMap$CaseDiff / ItalyMap$pop
  ITALY_DATA <- subset(ItalyMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(ITALY_DATA)
}
