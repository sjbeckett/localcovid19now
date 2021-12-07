
geomWorld <- st_read("countries/data/WorldPreSimp/geomWorld_presimp.geojson")
# bind_rows new internally valid countries to geomWorld

geomGlobal <- ms_simplify(geomWorld,keep=0.05,keep_shapes=TRUE)%>%
  st_make_valid()

st_write(geomGlobal, "countries/data/geom/geomGlobal.geojson", delete_dsn = T)

# europeCountries <- st_read("countries/data/temp_geom/geomEurope.geojson")%>%
#   .$m49code%>%
#   unique()

groupedGlobal <- geomGlobal%>%
  group_by(filename)


globalList <- group_split(groupedGlobal, .keep = F)
names(globalList) <- pull(group_keys(groupedGlobal))

for(y in seq_along(globalList)){
  assign(x=names(globalList[y]), value = globalList[y][[1]])
  st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/",names(globalList[y]),".geojson"))
  rm(names(globalList[y]))
}