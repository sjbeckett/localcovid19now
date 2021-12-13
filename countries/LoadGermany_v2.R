LoadGermany_v2 <- function(){
links <- c('https://opendata.arcgis.com/datasets/8a0b7d7c9fb442ffaa512221cf11366e_0.geojson', #Baden-Württemberg
          'https://opendata.arcgis.com/datasets/45258e51f57d43efb612f700a876ae8f_0.geojson', # bayern
          'https://opendata.arcgis.com/datasets/3949d6fd2dc74386b763e451f4c6e384_0.geojson', # Berlin
          'https://opendata.arcgis.com/datasets/5f81692e203a4888a64cb1976aafbd34_0.geojson', #Brandenburg
          'https://opendata.arcgis.com/datasets/f7bdcbe7188545daabe65e6c9e2a4379_0.geojson', #Bremen
          'https://opendata.arcgis.com/datasets/ab2c1b9c36734faf937cd83dee339517_0.geojson', #Hamburg
          'https://opendata.arcgis.com/datasets/3ed997d4a8a447f09ab122a1a432b070_0.geojson', #Hessen
          'https://opendata.arcgis.com/datasets/d6c27576ee034bb78621012738615598_0.geojson', #Mecklenburg-Vorpommern
          'https://opendata.arcgis.com/datasets/14d82a9addf841789cd6ef5c1f67476a_0.geojson', #Niedersachsen/Lower Saxony
          'https://opendata.arcgis.com/datasets/a99afefd4258435f8af660b6cbed9bf7_0.geojson', #Nordrhein-Westfalen
          'https://opendata.arcgis.com/datasets/57e385f51a07495cb0a1e00a55ee1b5b_0.geojson', #Rheinland-Pfalz
          'https://opendata.arcgis.com/datasets/0e59e1262dba4f5f8d6a904113bf7c99_0.geojson', #Saarland
          'https://opendata.arcgis.com/datasets/3d3235c08d4f44a2afd088546b704902_0.geojson', #Sachsen
          'https://opendata.arcgis.com/datasets/06a1c943a9b845968b5ad0607f5f48f5_0.geojson', #Sachsen-Anhalt
          'https://opendata.arcgis.com/datasets/4a648483aedd49b8a6655290181d4c2a_0.geojson', #Schleswig-Holstein
          'https://opendata.arcgis.com/datasets/790f5423e03e49c4baec55a1a232c136_0.geojson' #Thüringen
          ) 
germanyData <- data.frame()
### recommendation to use geojsonsf::geojson_sf instead of st_read
for (link in links){
  regionData <- as.data.frame(st_read(link)) %>%
    select(Region = Landkreis, Cases = AnzahlFall, Date = Meldedatum, Id_region = IdLandkreis)
  print(link)
  germanyData <- rbind(germanyData,regionData)
}
germanyData$Date <- as.Date(germanyData$Date)
germanyData <- germanyData[order(germanyData$Date),]
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
#pop <- read.csv('C:/Users/Laptop88/Desktop/COVID-19/New COVID/subregionalcovid19/countries/data/germanyPop.csv')
pop <- read.csv('countries/data/germanyPop.csv')
# turn Id Landkreis columns into numeric form:
pop$IdLandkreis <- as.numeric(pop$IdLandkreis)

#SK Eisenach (16056) is reported with LK Wartburgkreis (16063) (according to RKI)
pop$Population[which(pop$IdLandkreis=="16063")] = pop$Population[which(pop$IdLandkreis=="16056")] + pop$Population[which(pop$IdLandkreis=="16063")]

# Geometry
#geomGermany <- st_read('https://public.opendatasoft.com/explore/dataset/covid-19-germany-landkreise/download/?format=geojson&timezone=Europe/Berlin&lang=en')
#geomGermany <- geomGermany[,c('county','geometry')]
#alter names to match the case dataset
#geomGermany$county[geomGermany$county=="StÃ¤dteregion Aachen"] <- "StÃ¤dteRegion Aachen"
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
head(GERMANY_DATA)
return(GERMANY_DATA)
}

