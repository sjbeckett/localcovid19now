LoadIndia <- function(){
#Data collated by covid19india.org from state bulletins and official reports: https://www.covid19india.org

data <- read_json('https://api.covid19india.org/v4/timeseries.json')
#consider switching with https://api.covid19india.org/v4/min/timeseries.min.json

stateList <- names(data)
dataTable <- data.frame(Date = as.character(), Code = as.character(),Difference = as.numeric())
for (i in 1:length(stateList)){
  state <- stateList[i]
  lenOfDay <- length(names(data[i][[1]]$dates))
  pastIndex <- lenOfDay - 14
  date <- names(data[i][[1]]$dates[lenOfDay])
  ## there may be some null data for confirmed for current day or 14 days ago
  if (!is.null(data[i][[1]]$dates[lenOfDay][[1]]$total$confirmed) && !is.null(data[i][[1]]$dates[pastIndex][[1]]$total$confirmed)){
    currentCases <- data[i][[1]]$dates[lenOfDay][[1]]$total$confirmed
    pastCases <- data[i][[1]]$dates[pastIndex][[1]]$total$confirmed
    difference <- (currentCases - pastCases)/14*10
  }else{
    difference <- NA
  }
  vec <- data.frame(Date = date, Code = state, Difference = difference)
  dataTable <- rbind(dataTable,vec)
}
## population file
pop <- read_json('https://api.covid19india.org/misc.json')
populationTable <- data.frame(Code = as.character(), State = as.character(), Population = as.numeric())
for (i in 1:length(pop$state_meta_data)){
  code <- pop$state_meta_data[[i]]$abbreviation # take the state code
  population <- pop$state_meta_data[[i]]$population # extract the corresponding population
  state <- pop$state_meta_data[[i]]$stateut
  vec <- data.frame(Code = code, State = state, Population = population)
  populationTable <- rbind(populationTable,vec)
}

indiadf <- inner_join(dataTable,populationTable, by = c("Code"))


# GEOGRAPHIC GEOJSON FILE
#geomIndia <- st_read('https://raw.githubusercontent.com/divya-akula/GeoJson-Data-India/master/India_State.geojson')
#geomIndia <- geomIndia[,c("NAME_1","geometry")]
#geomIndia[geomIndia$NAME_1 == "Andaman and Nicobar",'NAME_1'] <- 'Andaman and Nicobar Islands'
#geomIndia[geomIndia$NAME_1 == "NCT of Delhi",'NAME_1'] <- 'Delhi'
#geomIndia[geomIndia$NAME_1 == "Dadra and Nagar Haveli",'NAME_1'] <- "Dadra and Nagar Haveli and Daman and Diu"
#geomIndia[geomIndia$NAME_1 == "Daman and Diu",'NAME_1'] <- "Dadra and Nagar Haveli and Daman and Diu"
geomIndia<-st_read("countries/data/geom/geomIndia.geojson")


## Citation
#' @misc{covid19indiaorg2020tracker,
#'   author = {COVID-19 India Org Data Operations Group},
#'   title = ,
#'   howpublished = {Accessed on yyyy-mm-dd from \url{https://api.covid19india.org/}},
#'   year = 2020
#' }

indiaMap <- inner_join(geomIndia,indiadf,by = c('micro_name' = 'State'))
indiaMap$DateReport =  as.character(indiaMap$Date)
indiaMap$RegionName = paste(indiaMap$micro_name, indiaMap$country_name, sep=", ")
indiaMap$Country = indiaMap$country_name
indiaMap$pInf = as.numeric(indiaMap$Difference)/as.numeric(indiaMap$Population)
india_DATA = subset(indiaMap,select=c("DateReport","RegionName","Country","pInf","geometry"))
return(india_DATA)
}