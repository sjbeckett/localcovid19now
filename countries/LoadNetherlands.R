LoadNetherlands <- function(){
#Covid-19 numbers per municipality as of publication date. RIVM / I & V / EPI. OSIRIS General Infectious Diseases (AIZ). https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/5f6bc429-1596-490e-8618-1ed8fd768427?tab=general


data <- vroom('https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv', delim = ';')#,fileEncoding = 'UTF-8')
netherlandsData <- data[,c('Date_of_publication','Municipality_code','Municipality_name','Province','Total_reported')]
names(netherlandsData) <- c('Date','Code','Municipality','Province','Cases')
netherlandsData$Date <- as.Date(netherlandsData$Date)
### Municipalities:
municipality <- unique(netherlandsData$Municipality)
municipality <- municipality[1:(length(municipality)-1)]
getData <- function(code){
  temp <- netherlandsData %>% filter(Municipality == municipality[code])
  temp$CumSum <- cumsum(temp$Cases)
  today <- temp$Date[length(temp$Date)]
  past_date <- today - 14
  pastData <- temp[temp$Date <= past_date,]
  ### SOME ROWS DO NOT REPORT MUNICIPALITY NAME
  difference <- (temp$CumSum[length(temp$CumSum)] - pastData$CumSum[length(pastData$CumSum)])/14*10
  vec <- data.frame(Municipality = municipality[code], Code = temp$Code[1], Date = today, Difference = difference)
  return(vec)
}

netherlandsTable <- data.frame()
for (i in 1:length(municipality)){
  vec <- getData(i)
  netherlandsTable <- rbind(netherlandsTable,vec)
}

netherlandsTable$Municipality[netherlandsTable$Municipality == "'s-Gravenhage" ] <- "Des Gravenhage"
# # Population data:
# dutchPop <- cbs_get_data(
#   id = "37259eng",
#   Periods = "2019JJ00",
#   CPI = "000000",
#   Regions = has_substring("GM"),
#   Sex = "T001038"
# )
# dutchPop <- dutchPop[,c("Regions", "PopulationOn1January_1")]
# names(dutchPop) <- c("Code","Bevolkingsaantal")
# 
# netherlandsTable <- inner_join(netherlandsTable, dutchPop)

### Geometry:
#geomNetherlands <- st_read('https://opendata.arcgis.com/datasets/620c2ab925f64ed5979d251ba7753b7f_0.geojson')
# Note that geomNetherlands$Bevolkingsaantal is population size.
geomNetherlands = st_read("countries/data/geom/geomNetherlands.geojson")
netherlandsMap <- inner_join(geomNetherlands, netherlandsTable, by = c("Gemeentecode"="Code"))
netherlandsMap$RegionName = paste0(netherlandsMap$Municipality,", Netherlands")
netherlandsMap$Country = "Netherlands"
netherlandsMap$DateReport = as.character(netherlandsMap$Date)
netherlandsMap$pInf = netherlandsMap$Difference/netherlandsMap$Bevolkingsaantal
NETHERLANDS_DATA = subset(netherlandsMap,select=c("DateReport","RegionName","Country","pInf"))

return(NETHERLANDS_DATA)
}