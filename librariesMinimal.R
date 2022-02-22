library(sf)
library(leaflet)
library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)
library(httr)
library(rvest)
library(rmapshaper) #for the ms_simplify function - simplifies polygons.
library(stringr)
library(vroom)
library(htmlwidgets)
library(purrr)
library(shiny)
library(RColorBrewer)
library(rgdal)
library(readxl)

library(COVID19) #for Chile, Peru,..
# removed data.table, fread replaced with vroom

#for Philippines
library(googledrive)
library(pdftools)

#for static maps
library(tmap)