#library("localcovid19now") #if not already installed

## Functionality is provided to load in data for a particular country, for example:

US <- LoadUS()

Malaysia <- LoadMalaysia()

#Note that to load data for the Philippines the googledrive package is used, requiring users google credentials.
#See more here: https://googledrive.tidyverse.org/reference/drive_auth.html
#credentials can be set before running the function:
googledrive::drive_auth(email = TRUE)

Philippines <- LoadPhilippines()

## or to load all available datasets as:
  
GLOBALMAP <- LoadCountries()
#can additionally set data older than 30 days, or with negative or zero differences in active cases to NA.
GLOBALMAP <- tidy_Data(GLOBALMAP) 


## Once loaded, the per capita active case data can be mapped with built in helper functions: 
  
#show per capita active cases per 100,000 people in leaflet
PerCapitaMap_leaflet(GLOBALMAP,100000)

#show per capita active cases per 100,000 people in tmap
PerCapitaMap_tmap(GLOBALMAP,100000)

GMAP = PerCapitaMap_tmap(GLOBALMAP,100000)
tmap::tmap_save(GMAP,"Global_pc_tmap.png")

## Additionally, with a provided assumed ascertainment bias (ratio between true infections and recorded cases), the risk that one or more people in a group of a particular size can be estimated and mapped:
  
#assume an ascertainment bias of 4 infections per recorded case.
US$AB <- 4 
#can additionally set data older than 30 days, or with negative or zero differences in active cases to NA.
US <- tidy_Data(US)

#show risk that one or more people are infectious in a group of 50 using leaflet
EventMap_leaflet(US,50,US$AB)

#show risk that one or more people are infectious in a group of 100 using tmap
EventMap_tmap(US,100,US$AB)

#maps can be saved using tmap commands. Here we also use a projection more suited to the US:
MAP = EventMap_tmap(US,100,US$AB,projection="ESPG:5070")
tmap::tmap_save(MAP,"US_RiskMap.png")

#we can also create a table of event risk by location as:
create_c19r_data(df_in = US, risk_output = "USrisk.csv", asc_bias_list = cbind(AB4 = US$AB))

## We caution that the case ascertainment bias may differ both across different regions and across time (due to differences in testing strategies).
