LoadCzechia <- function(){
#COVID-19 data sourced from National Health Information System, Regional Hygiene Stations, Ministry of Health of the Czech Republic and prepared by the Institute of Health Information and Statistics of the Czech Republic and the Institute of Biostatistics and Analyses, Faculty of Medicine, Masaryk University: https://onemocneni-aktualne.mzcr.cz/covid-19 

#Komenda M., Karolyi M., Bulhart V., Žofka J., Brauner T., Hak J., Jarkovský J., Mužík J., Blaha M., Kubát J., Klimeš D., Langhammer P., Daňková Š ., Májek O., Bartůňková M., Dušek L. COVID 19: Overview of the current situation in the Czech Republic. Disease currently [online]. Prague: Ministry of Health of the Czech Republic, 2020. Available from: https://onemocneni-aktualne.mzcr.cz/covid-19 . Development: joint workplace of IHIS CR and IBA LF MU. ISSN 2694-9423.

#Geometry
#geomCzechia <<- st_read('https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/distictsCzechiaLow.json') 
geomCzechia <- st_read("countries/data/geom/geomCzechia.geojson")
# geomCzechia <- geomCzechia %>% select(name, geometry) %>% mutate(name = as.character(name))

#population
czech_pop <- vroom('countries/data/czech_pop.csv')
# names(czech_pop)[1] <- 'code'

#case data
czechData <- vroom('https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/kraj-okres-nakazeni-vyleceni-umrti.csv')

names(czechData) <- c('id','Date','Code','District','Confirmed','Cure','Death')

czechData$Date <- as.Date(czechData$Date)
czechData = czechData %>% 
    group_by(District) %>% 
    slice(c(n(), n()-14)) %>% 
    summarize(cases = (Confirmed[1]-Confirmed[2])*10/14, Date = first(Date)) %>% 
    ungroup

#integrate datasets  
  czech_data_join <- inner_join(as.data.frame(czechData), czech_pop, by = c("District" = "Code"))
  names(czech_data_join) <- c('Code','Difference','Date','name','Population')
  CzechMap <- inner_join(geomCzechia,czech_data_join, by = c("micro_name"='name'))
  
  CzechMap$RegionName = paste(CzechMap$micro_name,CzechMap$country_name, sep=", ")
  CzechMap$Country = CzechMap$country_name
  CzechMap$DateReport = as.character(CzechMap$Date) 
  CzechMap$pInf = CzechMap$Difference/CzechMap$Population
  
  CZECH_DATA = subset(CzechMap,select=c("DateReport","RegionName","Country","pInf","geometry"))
  return(CZECH_DATA)
}
  
