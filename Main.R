

source('librariesMinimal.R')
#source mapping utilities
source("MappingFunctions.R")


source("countries/LoadPhilippines.R")
PHILIPPINES = LoadPhilippines()

#set ascertainment bias
PHILIPPINES$AB = 3
EventMap(PHILIPPINES,50)
