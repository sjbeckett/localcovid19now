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

library(shiny)
library(RColorBrewer)
library(rgdal)

library(COVID19) #for Chile, Peru,..
library(data.table) #for the fread function (large speed up vs. read.csv for large files). Just using this for the Germany dataset right now, but could be useful for other files also.

#for Philippines
library(googledrive)
library(pdftools)