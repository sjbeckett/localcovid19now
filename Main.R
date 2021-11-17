

source('librariesMinimal.R')
#source mapping utilities
source("MappingFunctions.R")


source("countries/LoadPhilippines.R")
PHILIPPINES = LoadPhilippines(oauth="philippinescovid.json") # where "philippinescovid.json" is the path to an oauth item from google api

#set ascertainment bias
PHILIPPINES$AB = 3
EventMap(PHILIPPINES,50)