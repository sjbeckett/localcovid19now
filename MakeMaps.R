# Load packages
#source libraries
source('librariesMinimal.R')

#source local files
#source country by country files
source("sourceCountryList.R")
#source mapping utilities
source("MappingFunctions.R")


###LOAD IN DATA
#COMBINE DATASETS INTO SINGLE OBJECT
GLOBALMAP <- LoadCountries()
GLOBALMAP <- ms_simplify(GLOBALMAP,keep=0.05,keep_shapes=TRUE)

###Append ascertainment bias calculations to each region via country level statistics
#APPEND AB to country.
GLOBALMAP$AB = 3
#let's revisit the dynamic stuff a little later


#No data if pInf = 0
GLOBALMAP2 = GLOBALMAP
GLOBALMAP2$pInf[GLOBALMAP2$pInf==0]=NA
#Subset to only include recent data
GLOBALMAP3 = GLOBALMAP2
GLOBALMAP3$pInf[which(GLOBALMAP2$DateReport < (Sys.Date()-30))] = NA


#save map
st_write(GLOBALMAP,"GlobalRiskMapping_ABD_18October2021.geojson")

###plot map

#Map with 50 people
MapTogether = EventMap(GLOBALMAP3,50)
MapTogether

###save as widget

library(htmlwidgets)
saveWidget(MapTogether,"GlobalRiskMapping_ABD_100_11August2021.html",selfcontained=T) 
