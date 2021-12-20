library(rmapshaper) #for the ms_simplify function - simplifies polygons.
temp <- unzip(zipfile = "countries/data/WorldPreSimp.zip", exdir = tempdir() )
geomWorld <- st_read(temp)
unlink(temp)

# bind_rows new internally valid countries to geomWorld

geomGlobal <- ms_simplify(geomWorld,keep=0.05,keep_shapes=TRUE)%>%
  st_make_valid()

st_write(geomGlobal, "countries/data/geom/geomGlobal_simplified.geojson", delete_dsn = T)

groupedGlobal <- geomGlobal%>%
  group_by(filename)

globalList <- group_split(groupedGlobal, .keep = F)
names(globalList) <- pull(group_keys(groupedGlobal))

for(y in seq_along(globalList)){
  assign(x=names(globalList[y]), value = globalList[y][[1]])
  st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/",names(globalList[y]),".geojson"))
  rm(list=names(globalList[y]))
}
