LoadGermany <- function() {
#Number of cases in Germany from the Robert Koch-Institut and the Bundesamt für Kartographie und Geodäsie: https://npgeo-corona-npgeo-de.hub.arcgis.com/
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/about


#GERMANY_SOURCE_Data <-  data.frame(st_read('https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson')) #note this is a large file > 1GB.
#alternative:
#GERMANY_SOURCE_Data = fread("https://opendata.arcgis.com/api/v3/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/downloads/data?format=csv&spatialRefId=4326", encoding="UTF-8") # file size > 220MB.
# GERMANY_SOURCE_Data = vroom::vroom("https://opendata.arcgis.com/api/v3/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/downloads/data?format=csv&spatialRefId=4326") # file 
  
  # download file and then read in with vroom
  temp = tempfile()
  download.file(url = "https://opendata.arcgis.com/api/v3/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/downloads/data?format=csv&spatialRefId=4326", destfile = temp)
  GERMANY_SOURCE_Data = vroom::vroom(temp)
  unlink(temp)

germanyData <- GERMANY_SOURCE_Data[,c("Landkreis","AnzahlFall","Meldedatum",'IdLandkreis')] #note that cases are split by age group and gender
# rename columns
names(germanyData) <- c('Region', 'Cases', 'Date','Id_region')
germanyData$Date <- as.Date(germanyData$Date)
germanyData <- germanyData[order(germanyData$Date),]

##find place and date, sum across age and gender groups

#get the county list
County <- unique(germanyData$Region)

## find out the difference in 10 days correction for each county from 14 days difference
germany_sortFunc <- function(code){
  dataSet <- germanyData %>% filter(germanyData$Region == County[code])
  dataSet$CumSum <- cumsum(dataSet$Cases)
  latestDate <- dataSet$Date[length(dataSet$Date)]
  pastDate <- latestDate - 14
  pastSet <- dataSet[which(dataSet$Date <= pastDate),] # there are some missing days in the past, so we have to choose the close day
  X = as.numeric((latestDate - pastSet$Date[length(pastSet$Date)]))  # get how many days difference
  #print(X) 14 and 15's - must be 14 or more!
  if (X > 2){ #?
    difference <- ((max(dataSet$CumSum) - max(pastSet$CumSum))*10/X)
  } else {
    difference <- NA
  }
  vec <- data.frame(County = County[code],Date = latestDate, Difference = difference, IdLandkreis = dataSet$Id_region[1])
  return(vec)
}

## get the data table contains landkreis, updated date and difference in 14 days
germanyTable <- data.frame()
for (ii in 1:length(County)){
  vec <- germany_sortFunc(ii)
  germanyTable <- rbind(germanyTable,vec)
}
germanyTable$IdLandkreis <- as.numeric(germanyTable$IdLandkreis)

## POP: https://www.citypopulation.de/en/germany/admin/
pop <- read.csv('countries/data/germanyPop.csv')
# turn Id Landkreis columns into numeric form:
pop$IdLandkreis <- as.numeric(pop$IdLandkreis)

#SK Eisenach (16056) is reported with LK Wartburgkreis (16063) (according to RKI)
pop$Population[which(pop$IdLandkreis=="16063")] = pop$Population[which(pop$IdLandkreis=="16056")] + pop$Population[which(pop$IdLandkreis=="16063")]

# Geometry
#geomGermany <- st_read('https://public.opendatasoft.com/explore/dataset/covid-19-germany-landkreise/download/?format=geojson&timezone=Europe/Berlin&lang=en')
#geomGermany <- geomGermany[,c('county','geometry')]
#alter names to match the case dataset
#geomGermany$county[geomGermany$county=="Städteregion Aachen"] <- "StädteRegion Aachen"
#IND1 = which(geomGermany$county=="SK Eisenach")
#IND2 = which(geomGermany$county=="LK Wartburgkreis")
#HMM<- st_union(geomGermany[c(IND1,IND2),])%>% st_cast("MULTIPOLYGON")
#geomGermany$geometry[IND2] = HMM
#geomGermany = geomGermany[-IND1,]
geomGermany <- st_read("countries/data/geom/geomGermany.geojson")



#integrate datasets
germanydf <- inner_join(germanyTable,pop, by = 'IdLandkreis') # add pop column into the main germany table
germanyMap <- inner_join(geomGermany, germanydf, by = c("county" = "County")) #link to geometry

germanyMap$DateReport =  as.character(germanyMap$Date)
germanyMap$RegionName = paste0(germanyMap$county,', Germany')
germanyMap$Country = "Germany"
germanyMap$pInf = germanyMap$Difference/germanyMap$Population
GERMANY_DATA = subset(germanyMap,select=c("DateReport","RegionName","Country","pInf","geometry"))

return(GERMANY_DATA)
}