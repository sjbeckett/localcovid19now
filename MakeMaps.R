# Load packages
#source libraries
source('librariesMinimal.R')

#source local files
#source country by country files
country_source_files = list.files('./countries/', pattern = "*.R", full.names = T)
sapply(country_source_files, \(x) source(x, encoding = "UTF-8"))
#source mapping utilities
source("MappingFunctions.R")

###LOAD IN DATA
# define 'oauth' as the file path to the oauth json file.
#COMBINE DATASETS INTO SINGLE OBJECT
GLOBALMAP <- LoadCountries()

# Create world_risk_regions.csv
# risk < 1% -> 0 
source("MakeTable.R")
create_c19r_data(st_drop_geometry(GLOBALMAP)%>%
  as_tibble())

###Append ascertainment bias calculations to each region via country level statistics


#APPEND AB to country.
GLOBALMAP$AB = 3
#save map
st_write(GLOBALMAP,sprintf("GlobalRiskMapping_ABD_%s.geojson",filedate), delete_dsn = T)

#No data if pInf = 0
GLOBALMAP$pInf[GLOBALMAP$pInf==0]=NA
#Subset to only include recent data
GLOBALMAP$pInf[which(GLOBALMAP$DateReport < (Sys.Date()-30))] = NA

#Map with 50 people
MapTogether = EventMap(GLOBALMAP,50)
if (interactive())
    MapTogether

# today's date
filedate <- paste(day(today()), month(today(), label=T, abbr = F), year(today()), sep = "")
htmlwidgets::saveWidget(MapTogether,sprintf("GlobalRiskMapping_ABD_100_%s.html", filedate),selfcontained=T) 
