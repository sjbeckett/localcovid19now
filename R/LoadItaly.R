#' dataQueryItaly
#'
#' @param date search for Italian COVID-19 data on this date
#'
#' @return returns data for Italy on specified date. Called by LoadItaly().
#' @keywords internal
dataQueryItaly <- function(date) {
  data <- vroom(paste0("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-", str_replace_all(as.character(date), "-", ""), ".csv"), col_types = cols(note = col_character())) %>%
    select(date = data, region = denominazione_regione, province = denominazione_provincia, code = codice_provincia, cases = totale_casi)
  return(data)
}

#' LoadItaly
#'
#' @description Reads in subnational data for Italy to calculate most recent estimate of per capita active COVID-19 cases.
#'
#' @note
#' Data collated by the Italian Department of Civil Protection COVID-19: \url{https://github.com/pcm-dpc/COVID-19/}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Italy <- LoadItaly()
#' }
#' @seealso [LoadCountries()]
#' @export
LoadItaly <- function() {
  # Italian Department of Civil Protection COVID-19 Data: https://github.com/pcm-dpc/COVID-19/

  # italy: need to download data_cur and data_past respectively
  cur_date <- ymd(gsub("-", "", Sys.Date())) - 1
  # past_date <- ymd(cur_date) - 14
  #
  # data_past <- dataQueryItaly(past_date) %>%
  #   select(date, code, cases) # date, abbreviation_canton_and_fl, ncumul_conf
  data_cur <- dataQueryItaly(cur_date)
  for (i in c(1:13)) {
    data_cur <- data_cur %>% rbind(dataQueryItaly(cur_date - i))
  }
  data_cur <- data_cur %>%
    group_by(code) %>%
    dplyr::summarise(date = first(date), cases = first(cases), region = first(region), province = first(province), n = n())

  # This will actually give results inclusive of missing data
  past_date <- unique(as_date(data_cur$date)) - 14 # get all dates 14 days before a most recent current date

  data_past <- map_df(past_date, ~ dataQueryItaly(.x)) %>%
    select(date, code, cases) %>%
    inner_join( # only keep the past dates that are 14 days before their corresponding date
      data_cur %>%
        select(code, date) %>%
        mutate(date = date - period(14, "days"))
    )


  # population
  # pop <- vroom("countries/data/italyPop.csv", col_types = cols(code = col_character())) %>%
  #   mutate(
  #     code = str_pad(code, width = 3, side = "left", pad = "0")
  #   )
  data("pop_italy")

  data_join <- data_cur %>%
    inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    inner_join(pop_italy, by = c("code"))
  data_join <- as.data.frame(data_join)
  data_join$CaseDiff <- (data_join$cases - data_join$cases_past) * 10 / 14
  data_join$date <- as_date(data_join$date)

  # geometry
  # geom <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/italy_simpler.geojson")
  data("geomItaly")
  # integrate datasets

  ItalyMap <- inner_join(geomItaly, data_join, by = c("micro_code" = "code"))
  ItalyMap$RegionName <- paste(ItalyMap$name, ItalyMap$country_name, sep = ", ")
  ItalyMap$Country <- ItalyMap$country_name
  ItalyMap$DateReport <- as.character(ItalyMap$date)
  ItalyMap$pInf <- ItalyMap$CaseDiff / ItalyMap$pop
  ITALY_DATA <- subset(ItalyMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(ITALY_DATA)
}
