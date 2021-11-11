LoadChina <- function(){
#COVID-19 data for first-level administrative divisions in China is aggregated from
#    National Health Commission of the People’s Republic of China (NHC): http://www.nhc.gov.cn/xcs/yqtb/list_gzbd.shtml
#    China CDC (CCDC): http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm
#by the COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University
#https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
#Dong, E., Du, H., & Gardner, L. (2020). An interactive web-based dashboard to track COVID-19 in real time. The Lancet infectious diseases, 20(5), 533-534.

#load cases data
data <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')

# get updated date:
date <- names(data)[length(names(data))]
date <- strsplit(date,'X')[[1]][2]
date <- as.Date(date, format = "%m.%d.%y")

chinaData <- data[data$Country.Region == 'China',]
len <- length(names(chinaData))
latestData <- chinaData[,c(1,(len-14),len)]
names(latestData) <- c('Province','Past','Today')
latestData[4] <- (latestData[3]-latestData[2])*10/14
latestData <- latestData[,c(1,4)]
names(latestData) <- c('Province','Difference')
## population file
pop <- read.csv('countries/data/chinaPopulation.csv')
chinadf <- inner_join(latestData,pop, by = c('Province' = 'Name'))

## geojson file
#geomChina <- st_read('https://raw.githubusercontent.com/stjacob/china_geojson/master/china.geojson')
geomChina<- st_read("countries/data/geom/geomChina.geojson")

geomChina <- geomChina[,c('NAME_1','geometry')]
#alter naming to match across datasets
geomChina$NAME_1[19] = "Inner Mongolia"
geomChina$NAME_1[20] = "Ningxia"
geomChina$NAME_1[33] = "Hong Kong"

#integrate datasets
ChinaMap <- inner_join(geomChina, chinadf, by = c("NAME_1" = "Province"))
ChinaMap$RegionName = paste0(ChinaMap$NAME_1,", China")
ChinaMap$Country = "China"
ChinaMap$DateReport = as.character(date) 
ChinaMap$pInf = ChinaMap$Difference/ChinaMap$Population

CHINA_DATA = subset(ChinaMap,select=c("DateReport","RegionName","Country","pInf","geometry"))
return(CHINA_DATA)
}