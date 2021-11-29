source("librariesMinimal.R")
geojson_list <- list.files("countries/data/geom/", full.names = T)

geoNames <-  list()
extract_altinfo <- function(geojson){
  if(str_detect(geojson,".geojson$")){
    fname <- str_extract(geojson,"[:alpha:]*(?=.geojson)")
    geo <- st_read(geojson)
    geoNames[fname] <- names(geo)
  }else{
    cat("\nnot geojson\n")
  }
  
}