LoadBelgium <-  function() {
#Sciensano, the Belgian institute for health: https://epistat.wiv-isp.be/covid/

getDate <- function(x,y){
  dates <- as.character(Sys.Date()-x)
  if(y==0){
	string <- unlist(strsplit(dates,'-'))
	new_date <- sprintf("%s%s%s",string[1],string[2],string[3])
  }else{
	new_date=dates
  }
  return(new_date)
}

#cases - loading individual files from historical datasets of cumulative cases at municipality level (581), which we will aggregate to Arrondissements (43).
#find latest data
flag=0
aa=0
while(flag==0){
	STRING = paste0('https://epistat.sciensano.be/Data/',getDate(aa,0),'/COVID19BE_CASES_MUNI_CUM_',getDate(aa,0),'.csv')
	latest_data <- try(read.csv(STRING,encoding="UTF-8"))
	if (is.null(dim(latest_data)) == FALSE){
		flag=1
	}else{
		aa=aa+1
	}
	if(aa>5){
		warning("no recent data")
		flag=2
	}
}

UpdateDate = getDate(aa,1)
#find past data
flag=0
aa=0
while(flag==0){
	STRING = paste0('https://epistat.sciensano.be/Data/',getDate(14+aa,0),'/COVID19BE_CASES_MUNI_CUM_',getDate(14+aa,0),'.csv')
	past_data <- try(read.csv(STRING,encoding="UTF-8"))
	if (is.null(dim(past_data)) == FALSE){
		flag=1
	}else{
		aa=aa+1
	}
	if(aa>5){
		warning("no recent data")
		flag=2
	}
}
PastDate = getDate(14+aa,1)
DateDiff = as.numeric(abs(diff(c( as.Date(UpdateDate), as.Date(PastDate))))) #difference in days between case records

#select columns of interest - NL and FR Arrondissement names and cumulative cases per municipality.
latest_data <- latest_data[,c('TX_ADM_DSTR_DESCR_NL','TX_ADM_DSTR_DESCR_FR','CASES')]
past_data <- past_data[,c('TX_ADM_DSTR_DESCR_NL','TX_ADM_DSTR_DESCR_FR','CASES')]

# get the list of arrondissment:
Arrondissement <- unique(latest_data$TX_ADM_DSTR_DESCR_NL) #43 Arrondissements and NA

# collect data for each arrondissment and assemble them into 1 table
getData <- function(dataSet){
  updated_data <- data.frame(Arrondissement = as.character(), Cases = as.numeric())
  for (i in 1:length(Arrondissement)){
    data <- dataSet %>% filter(dataSet$TX_ADM_DSTR_DESCR_NL == Arrondissement[i])
    if (sum(data$CASES == '<5') > 0){
      data[data$CASES == '<5','CASES'] <- 0 ## assume there is no case when the box shows <5
    }
    temp <- data.frame(Arrondissement = unlist(strsplit(Arrondissement[i],'Arrondissement '))[2],Cases = sum(as.numeric(data$CASES)))
    updated_data <-rbind(updated_data,temp)
  }
  return(updated_data)
}

latest_update <- getData(latest_data)
past_update <- getData(past_data)
difference <- (latest_update$Cases-past_update$Cases)*10/DateDiff

### DESIRED TABLE
finalData <- data.frame(Arrondissement = latest_update[,'Arrondissement'], Difference = difference)

# geojson file
#geomBelgium <- st_read('countries/data/geom/geomBelgium.geojson')
#geomBelgium <- geomBelgium[,c("NameDUT","geometry")]
#for (i in 1:length(geomBelgium$NameDUT)){
#  geomBelgium$NameDUT[i] <- unlist(strsplit(geomBelgium$NameDUT[i], "Arrondissement "))[2]
#}
#geomBelgium$NameDUT[28] <- sort(finalData$Arrondissement)[22] # La Louviere
#geomBelgium$NameDUT[39] <- sort(finalData$Arrondissement)[29] # Neufchâteau
#geomBelgium <- st_write(geomBelgium,'countries/data/geom/geomBelgium.geojson')
geomBelgium <- st_read('countries/data/geom/geomBelgium.geojson')

finalData <- inner_join(geomBelgium, finalData, by = c('NameDUT' = 'Arrondissement'))

#population
pop <- read.csv("countries/data/belgiumPopulation.csv",encoding="UTF-8")

#add to dataset
belgiumdf <- inner_join(finalData,pop, by = c('NameDUT' = 'Name'))


# Change names to account for NL and FR:
nameMuni <- read.csv('countries/data/belgiumNames.csv')
for (i in 1:length(belgiumdf$NameDUT)){
  for (j in 1:length(nameMuni$Dutch.name)){
    if (belgiumdf$NameDUT[i] == nameMuni$Dutch.name[j] & nameMuni$French.name[j] != '-'){
      belgiumdf$NAME[i] <- paste0(belgiumdf$NameDUT[i],'/',nameMuni$French.name[j])
    }
    else if (belgiumdf$NameDUT[i] == nameMuni$French.name[j] & nameMuni$Dutch.name[j] != '-'){
      belgiumdf$NAME[i] <- paste0(belgiumdf$NameDUT[i],'/',nameMuni$Dutch.name[j])
    }
    else if ((belgiumdf$NameDUT[i] == nameMuni$Dutch.name[j] | belgiumdf$NameDUT[i] == nameMuni$French.name[j]) & (nameMuni$French.name[j] == '-' | nameMuni$Dutch.name[j] == '-')){
      belgiumdf$NAME[i] <- belgiumdf$NameDUT[i]
    }
  }
  if (belgiumdf$NameDUT[i] == 'Tournai-Mouscron'){
    belgiumdf$NAME[i] <- paste0('Tournai/Doornik-Mouscron/Moeskroen')
  }
}

#integrate datsets
belgiumdf$RegionName = paste0(belgiumdf$NAME, ", Belgium")
belgiumdf$Country = "Belgium"
belgiumdf$DateReport = as.character(UpdateDate) 
belgiumdf$pInf = belgiumdf$Difference/belgiumdf$Population

BELGIUM_DATA = subset(belgiumdf,select=c("DateReport","RegionName","Country","pInf","geometry"))
return(BELGIUM_DATA)
}
