LoadSwitzerlandLiechtenstein <- function() {
#Specialist Unit for Open Government Data Canton of Zurich COVID-19 data: https://github.com/openZH/covid_19

  data <- read.csv("https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv", stringsAsFactors = FALSE) %>%
    mutate(date = as_date(date)) %>%
    arrange(desc(date)) %>%
    filter(!is.na(ncumul_conf)) %>%
    select(date = date, code = abbreviation_canton_and_fl, cases = ncumul_conf)
  
  #geometry
  #geom <<- st_read("https://gist.githubusercontent.com/mbostock/4207744/raw/3232c7558742bab53227e242a437f64ae4c58d9e/readme-swiss.json")
  #geom <- st_set_crs(geom, 4326)
  #geom <- st_read("countries/data/geom/geomSwitzerland.geojson")
  geom <- st_read("countries/data/geom/geomSwitzerlandLiechtenstein.geojson")
  
  #population
  #pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/swiss_canton_pop.csv", stringsAsFactors = FALSE)
  pop <- read.csv("countries/data/Switzerland_pop.csv")
  
  cur_date <- data$date[1]
  past_date <- ymd(cur_date) - 14
  data_cur <<- data %>%
    group_by(code) %>%
    summarise(code = first(code), cases = first(cases), date = first(date)) %>%
    as.data.frame()
  data_past <- data %>%
    filter(date <= past_date) %>%
    group_by(code) %>%
    summarise(code = first(code), cases = first(cases), date = first(date)) %>%
    as.data.frame()
  data_join <<- data_cur %>%
    inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("code")) %>%
    mutate(n = date - date_past) %>%
    select(-c("name"))
  data_join$n <- as.numeric(data_join$n)
  data_join$CaseDiff <- if_else(data_join$n != 0, (data_join$cases-data_join$cases_past)*10/data_join$n, NA_real_)
  
  #integrate datasets
  SwitzerlandMap <- inner_join(geom,data_join, by = c("id" = 'code'))
  SwitzerlandMap$RegionName = paste0(SwitzerlandMap$name,", Switzerland")
  SwitzerlandMap$RegionName[27] = paste0(SwitzerlandMap$name[27],", Liechtenstein")
  SwitzerlandMap$Country = "Switzerland"
  SwitzerlandMap$DateReport = as.character(SwitzerlandMap$date) 
  SwitzerlandMap$pInf = SwitzerlandMap$CaseDiff/SwitzerlandMap$pop
  
  SWITZERLAND_DATA = subset(SwitzerlandMap,select=c("DateReport","RegionName","Country","pInf","geometry"))
  
  return(SWITZERLAND_DATA)
  }