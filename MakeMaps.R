# Load packages
#source libraries
source('librariesMinimal.R')

#source local files
#source country by country files
country_source_files = list.files('./countries/', pattern = "*.R", full.names = T)
sapply(country_source_files, source)
#source mapping utilities
source("MappingFunctions.R")


###LOAD IN DATA
#COMBINE DATASETS INTO SINGLE OBJECT
GLOBALMAP <- LoadCountries()
GLOBALMAP <- ms_simplify(GLOBALMAP,keep=0.05,keep_shapes=TRUE)
###Append ascertainment bias calculations to each region via country level statistics
#APPEND AB to country.
GLOBALMAP$AB = 3
#save map
st_write(GLOBALMAP,"GlobalRiskMapping_ABD_18October2021.geojson")

#No data if pInf = 0
GLOBALMAP$pInf[GLOBALMAP$pInf==0]=NA
#Subset to only include recent data
GLOBALMAP$pInf[which(GLOBALMAP$DateReport < (Sys.Date()-30))] = NA

#Map with 50 people
MapTogether = EventMap(GLOBALMAP,50)
if (interactive())
    MapTogether


htmlwidgets::saveWidget(MapTogether,"GlobalRiskMapping_ABD_100_11August2021.html",selfcontained=T) 
