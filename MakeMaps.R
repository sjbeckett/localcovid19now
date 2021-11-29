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
# define 'oauth' as the file path to the oauth json file.
#COMBINE DATASETS INTO SINGLE OBJECT
GLOBALMAP <- LoadCountries()
GLOBALMAP <- ms_simplify(GLOBALMAP,keep=0.05,keep_shapes=TRUE)
###Append ascertainment bias calculations to each region via country level statistics
# today's date
filedate <- paste(day(today()), month(today(), label=T, abbr = F), year(today()), sep = "")
#APPEND AB to country.
GLOBALMAP$AB = 3
#save map
st_write(GLOBALMAP,sprintf("GlobalRiskMapping_ABD_%s.geojson",filedate))

#No data if pInf = 0
GLOBALMAP$pInf[GLOBALMAP$pInf==0]=NA
#Subset to only include recent data
GLOBALMAP$pInf[which(GLOBALMAP$DateReport < (Sys.Date()-30))] = NA

#Map with 50 people
MapTogether = EventMap(GLOBALMAP,50)
if (interactive())
    MapTogether


htmlwidgets::saveWidget(MapTogether,sprintf("GlobalRiskMapping_ABD_100_%s.html", filedate),selfcontained=T) 
