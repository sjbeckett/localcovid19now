LoadNorway <- function() {
#Thomas, Haarstad, F., Manuel & YBK. Public COVID-19 Data for Norway (covid19data.no). https://github.com/thohan88/covid19-nor-data

data <- read.csv('https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality.csv', fileEncoding = "UTF-8")
data$date <- as.Date(data$date)

## THIS SCRIPT ASSUMES ALL COUNTIES UPDATE AT ONCE.
today <- data$date[length(data$date)]
latest_data <- data %>% filter(data$date == today)
latest_data <- latest_data[,c('date','kommune_no','kommune_name','population','cases')]
past_data <- data %>% filter(data$date == (today - 14))
past_data <- past_data[,c('kommune_name','cases')]
norwaydf <- inner_join(latest_data,past_data, by = 'kommune_name')
norwaydf$Difference <- (norwaydf$cases.x - norwaydf$cases.y)*10/14
norwaydf <- norwaydf[,c('date','kommune_no','kommune_name','population','Difference')]

### Geojson:
#geomNorway <- st_read('https://github.com/smistad/konverter-norgeskart-projeksjon/releases/download/v2020/kommuner_komprimert.json')
#geomNorway$kommunenummer <- as.numeric(geomNorway$kommunenummer)
#geomNorway <- geomNorway[,c('kommunenummer','geometry')]
geomNorway<- st_read("countries/data/geom/geomNorway.geojson")

#integrate datasets
norwayMap <- inner_join(geomNorway, norwaydf, by = c("kommunenummer" = "kommune_no"))
norwayMap$DateReport =  as.character(norwayMap$date)
norwayMap$RegionName = paste0(norwayMap$kommune_name, ', Norway')
norwayMap$Country = "Norway"
norwayMap$pInf = norwayMap$Difference/norwayMap$population

NORWAY_DATA = subset(norwayMap,select=c("DateReport","RegionName","Country","pInf","geometry"))

return(NORWAY_DATA)
}