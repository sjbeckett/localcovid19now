

source('librariesMinimal.R')
#source mapping utilities
source("MappingFunctions.R")


source("countries/LoadNetherlands.R")
NETHERLANDS = LoadNetherlands()

#set ascertainment bias
NETHERLANDS$AB = 3
EventMap(NETHERLANDS,50)
