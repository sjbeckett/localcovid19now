LoadItaly <- function() {
#Italian Department of Civil Protection COVID-19 Data: https://github.com/pcm-dpc/COVID-19/

  dataQueryItaly <- function(date) {
    data <- read.csv((paste0("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-", str_replace_all(as.character(date), "-", ""), ".csv")), stringsAsFactors = FALSE) %>%
      select(date = data, region = denominazione_regione, province = denominazione_provincia, code = codice_provincia, cases = totale_casi)    
    return(data)
  }
  # italy: need to download data_cur and data_past respectively
  cur_date <- ymd(gsub("-", "", Sys.Date())) -1
  past_date <- ymd(cur_date) - 14
  
  data_past <- dataQueryItaly(past_date) %>%
    select(date, code, cases) # date, abbreviation_canton_and_fl, ncumul_conf
  data_cur <- dataQueryItaly(cur_date)
  for (i in c(1:13)) {
    data_cur <- data_cur %>% rbind(dataQueryItaly(cur_date - i))
  }
  data_cur <- data_cur %>%
    group_by(code) %>%
    dplyr::summarise(date = first(date), cases = first(cases), region = first(region), province = first(province), n = n())
 
#geometry 
  #geom <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/italy_simpler.geojson")
  geom <- st_read("countries/data/geom/geomItaly.geojson")
#population
  pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/italy_pop.csv", stringsAsFactors = FALSE)
  
  data_join <<- data_cur %>%
    inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("code"))
  data_join <- as.data.frame(data_join)
  data_join$CaseDiff <- (data_join$cases-data_join$cases_past)*10/14
  data_join$date <- as.Date(data_join$date)
  
#integrate datasets
  
  ItalyMap <- inner_join(geom,data_join, by = c("prov_istat_code_num" = 'code'))
  ItalyMap$RegionName = paste0(ItalyMap$name,", Italy")
  ItalyMap$Country = "Italy"
  ItalyMap$DateReport = as.character(ItalyMap$date) 
  ItalyMap$pInf = ItalyMap$CaseDiff/ItalyMap$pop
  ITALY_DATA = subset(ItalyMap,select=c("DateReport","RegionName","Country","pInf","geometry"))
  return(ITALY_DATA)
}