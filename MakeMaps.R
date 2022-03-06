# Load packages
# source libraries
source("librariesMinimal.R")

# source local files
# source country by country files
country_source_files <- list.files("./countries/", pattern = "*.R", full.names = T)
sapply(country_source_files, \(x) source(x, encoding = "UTF-8"))
# source mapping utilities
source("MappingFunctions.R")

### LOAD IN DATA

# COMBINE DATASETS INTO SINGLE OBJECT
## Note: a GoogleDrive oauth is required for the LoadPhilippines() function. It will prompt for the filepath
GLOBALMAP <- LoadCountries()

# Create world_risk_regions.csv
## risk < 1% -> 0
source("MakeTable.R")
GLOBALDAT <- st_drop_geometry(GLOBALMAP) %>%
  as_tibble()
create_c19r_data(GLOBALDAT = GLOBALDAT)

### Append ascertainment bias calculations to each region via country level statistics
GLOBALMAP$AB <- 3

# today's date
filedate <- paste(str_pad(day(today()),width = 2, side="left", pad="0"), month(today(), label = T, abbr = F), year(today()), sep = "")
# save map
st_write(GLOBALMAP, sprintf("GlobalRiskMapping_ABD_%s.geojson", filedate), delete_dsn = T)

# No data if pInf <= 0
GLOBALMAP$pInf[which(GLOBALMAP$pInf <= 0)] <- NA
# Subset to only include recent data
GLOBALMAP$pInf[which(GLOBALMAP$DateReport < (Sys.Date() - 30))] <- NA

# Map with 50 people
MapTogether <- EventMap_leaflet(GLOBALMAP, 50)
if (interactive()) {
  MapTogether
}


# Provides a csv of missing data for issue identification
GLOBALMAP%>%
  filter(is.na(pInf))%>%
  st_drop_geometry()%>%
  write.csv(file = sprintf("log_error/pInfNA_%s.csv", filedate), row.names = F)

htmlwidgets::saveWidget(MapTogether, sprintf("GlobalRiskMapping_ABD_50_%s.html", filedate), selfcontained = T)


#Example static maps via tmap
PCM = PerCapitaMap_tmap(GLOBALMAP,100000) #active cases per 100,000
tmap_save(PCM,sprintf("Global_pcm_per100000_%s.png", filedate))
EM = EventMap_tmap(GLOBALMAP,100) #risk for event of 100 people
tmap_save(EM,sprintf("Global_RiskMap_Ev100_%s.png", filedate))