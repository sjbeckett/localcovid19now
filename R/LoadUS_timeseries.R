#' LoadUS_alternate
#'
#' @description Reads in subnational data for the United States to calculate most recent estimate of active COVID-19 cases per day.
#' @inputs time window with which to save cases from
#'
#' @note
#' Real-time county level COVID19 data comes from the NYTimes COVID19 data project: \url{https://github.com/nytimes/covid-19-data}.
#' US 2019 population estimate data comes from the US Census: \url{https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html}.
#'
#' @return A simple feature returning the the recent data (DateReport), a unique region code (geoid), the region name (RegionName) 
#' and country name (Country), the number of active cases per day (new_cases),
#' the population size (pop), and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' US <- LoadUS()
#' }
#' @seealso [LoadCountries()]
#' @export
#'

LoadUS_v2 <- function(t_window = 364) {
    micro_code <- fips <- cases <- deaths <- date_past <- pop_usa <- NULL
    
    #Load in geometry and population data 
    utils::data("geomUnitedStates", envir = environment())
    utils::data("pop_usa", envir = environment())
    geomUnitedStates <- sf::st_as_sf(geomUnitedStates)
    
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
    county <- geomUnitedStates %>%
        dplyr::mutate(micro_code = as.numeric(micro_code))
    
    # county population level data
    # pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/county-population.csv", stringsAsFactors = FALSE)
    pop <- pop_usa
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
    past_date <- lubridate::ymd(cur_date) - t_window
    
    data_recent <- data %>%
        dplyr::filter(date <= cur_date, date>=past_date) %>%
        dplyr::mutate(fips = dplyr::case_when(
            county == "New York City" ~ 99999,
            county == "Kansas City" ~ 29991,
            county == "Joplin" ~ 29992,
            TRUE ~ as.numeric(fips)
        )) %>%
        dplyr::select(c(date, fips, cases, deaths)) %>%
        dplyr::inner_join(pop, by = "fips")
    #
    data_yesterday<-data %>%
        dplyr::filter(date <= cur_date -lubridate::days(1), date>= past_date-lubridate::days(1)) %>%
        dplyr::mutate(fips = dplyr::case_when(
            county == "New York City" ~ 99999,
            county == "Kansas City" ~ 29991,
            county == "Joplin" ~ 29992,
            TRUE ~ as.numeric(fips)
        )) %>%
        dplyr::select(c(date, fips, cases)) %>%
        dplyr::mutate(
            date = date +lubridate::days(1)
        ) %>%
        dplyr::rename(cases_yesterday = cases) 
    
    data_cur<- data_recent %>% 
        dplyr::inner_join(data_yesterday, by = c("date", "fips")) %>%
        dplyr::mutate(
            new_cases = cases - cases_yesterday
        ) %>%
        dplyr::select(date, fips, new_cases, pop)
    
    
    
    # integrate datasets
    USMap <- data_cur %>% dplyr::inner_join(county, by = c("fips" = "micro_code"))
    
    USMap$RegionName <- paste(USMap$micro_name, USMap$macro_name, USMap$iso3, sep = ", ")
    USMap$Country <- USMap$country_name
    USMap$DateReport <- as.character(USMap$date)
    US_DATA <- subset(USMap, select = c("DateReport", "geoid", "RegionName", "Country", "new_cases", "pop", "geometry"))
    US_DATA <- sf::st_as_sf(US_DATA)
    return(US_DATA)
}


# Test the function
#US_DATA <- LoadUS(t_window = 364)