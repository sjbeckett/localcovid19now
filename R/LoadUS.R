#' LoadUS
#'
#' @description Reads in subnational data for the United States to calculate most recent estimate of per capita active COVID-19 cases.
#'
#' @note
#' Real-time county level COVID19 data comes from the NYTimes COVID19 data project: \url{https://github.com/nytimes/covid-19-data}.
#' US 2019 population estimate data comes from the US Census: \url{https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' US <- LoadUS()
#' }
#' @seealso [LoadCountries()]
#' @export
LoadUS <- function() {
  # Real-time county level COVID19 data comes from the NYTimes COVID19 data project: https://github.com/nytimes/covid-19-data
  # US 2019 population estimate data comes from the US Census: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html

  # cases from NYT
  dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  # data <- read.csv(dataurl, stringsAsFactors = FALSE) %>% mutate(date = as_date(date))
  data <- vroom::vroom(dataurl, col_types = c(date = "D"))
  # geography
  # county <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/tl_2017_us_county.geojson")
  # merge counties that are reported together by the NYT
  # HMM<- st_union(county[c(2782,1612),])%>% st_cast("MULTIPOLYGON")
  # county$geometry[2782] = HMM
  # county$GEOID[2782] = 2997
  # county$NAME[2782] = "Bristol Bay plus Lake Peninsula"
  # HMM<- st_union(county[c(30,3049),])%>% st_cast("MULTIPOLYGON")
  # county$geometry[30] = HMM
  # county$GEOID[30] = 2998
  # county$NAME[30] = "Yakutat plus Hoonah-Angoon"
  county <- st_read("countries/data/geom/geomUnitedStates.geojson") %>%
    mutate(micro_code = as.numeric(micro_code))

  # county population level data
  # pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/county-population.csv", stringsAsFactors = FALSE)
  pop <- read.csv("countries/data/county_population.csv")
  # merge population for counties reported together by the NYT
  # bristol bay and Lake Peninsula
  IND <- which(pop$fips == 2164)
  IND2 <- which(pop$fips == 2060)
  pop$fips[IND] <- 2997
  pop$pop[IND] <- pop$pop[IND] + pop$pop[IND2]
  # Yakutat and Hoonah-Angoon
  IND <- which(pop$fips == 2282)
  IND2 <- which(pop$fips == 2105)
  pop$fips[IND] <- 2998
  pop$pop[IND] <- pop$pop[IND] + pop$pop[IND2]

  # calculate case differences
  cur_date <- data$date[length(data$date)]
  past_date <- ymd(cur_date) - 14

  data_cur <- data %>%
    filter(date == cur_date) %>%
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      county == "Kansas City" ~ 29991,
      county == "Joplin" ~ 29992,
      TRUE ~ as.numeric(fips)
    )) %>%
    select(c(date, fips, cases, deaths, ))
  data_past <- data %>%
    filter(date == past_date) %>%
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      county == "Kansas City" ~ 29991,
      county == "Joplin" ~ 29992,
      TRUE ~ as.numeric(fips)
    )) %>%
    select(date_past = date, fips = fips, cases_past = cases)
  data_join <- data_cur %>%
    inner_join(data_past, by = "fips") %>%
    inner_join(pop, by = "fips") %>%
    mutate(n = date - date_past)
  data_join$n <- as.numeric(data_join$n)
  data_join$CaseDiff <- (data_join$cases - data_join$cases_past) * 10 / data_join$n

  # integrate datasets
  USMap <- data_join %>% inner_join(county, by = c("fips" = "micro_code"))

  USMap$RegionName <- paste(USMap$micro_name, USMap$macro_name, USMap$iso3, sep = ", ")
  USMap$Country <- USMap$country_name
  USMap$DateReport <- as.character(USMap$date)
  USMap$pInf <- USMap$CaseDiff / USMap$pop
  US_DATA <- subset(USMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  US_DATA <- st_as_sf(US_DATA)
  return(US_DATA)
}
