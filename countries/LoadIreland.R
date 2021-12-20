LoadIreland <- function() {
#Data is provided by the Health Service Executive (HSE), Health Protection Surveillance Centre (HPSC), The Central Statistics Office (CSO) and Gov.ie and accessed via Ireland's COVID-19 Data Hub: https://covid19ireland-geohive.hub.arcgis.com/
  
  #geom <<- st_read('https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/Ireland_Counties.geojson')
  geom <- st_read("countries/data/geom/geomIreland.geojson")
  
  #Main COVID-19 hub page: https://covid-19.geohive.ie/datasets/d9be85b30d7748b5b7c09450b8aede63_0
  data <- vroom("https://opendata.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv") %>%
    mutate(date = as.Date(TimeStamp)) %>%
    select(CountyName, date, cases=ConfirmedCovidCases, pop = PopulationCensus16) %>%
    arrange(desc(date))
  data_cur <<- data %>%
    group_by(CountyName) %>%
    summarise(CountyName = first(CountyName), cases = first(cases), date = first(date), pop = first(pop)) %>%
    as.data.frame()
  past_date <- data_cur$date[1] - 14
  data_past <- data %>%
    filter(date == past_date) %>%
    group_by(CountyName) %>%
    summarise(CountyName = first(CountyName), cases = first(cases), date = first(date)) %>%
    as.data.frame()
  data_join <<- inner_join(data_cur, data_past, by = "CountyName", suffix=c('', '_past'))
  data_join$Difference <- (data_join$cases - data_join$cases_past)*10/14
  
  miscIreland <- vroom("countries/data/miscIreland.R")

#integrate datasets
  IrelandMap <- inner_join(geom,data_join, by = c('micro_name' = 'CountyName'))
  IrelandMap <- inner_join(IrelandMap, miscIreland, by = c("micro_code"="CO_ID"))
  IrelandMap$RegionName = paste(paste(IrelandMap$micro_name, IrelandMap$CONTAE, sep = "/"),IrelandMap$country_name, sep = ", ")
  IrelandMap$Country = IrelandMap$country_name
  IrelandMap$DateReport = as.character(IrelandMap$date) 
  IrelandMap$pInf = IrelandMap$Difference/IrelandMap$pop
  
  IRELAND_DATA = subset(IrelandMap,select=c("DateReport","RegionName","Country","pInf","geometry"))
  return(IRELAND_DATA)
}
