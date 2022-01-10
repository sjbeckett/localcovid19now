library(rmapshaper) #for the ms_simplify function - simplifies polygons.
temp <- unzip(zipfile = "countries/data/WorldPreSimp.zip", exdir = tempdir() )
geomWorld <- st_read(temp)
unlink(temp)

###
# bind_rows new internally valid countries to geomWorld

# Remove old geomSmallCountries and add new. This can be removed if updateGeoms.R is run again
geomSmallCountries <- st_read("countries/data/temp_geom/geomSmallCountries.geojson")%>%
  mutate(
    filename="geomSmallCountries",
    macro_code = "00",
    micro_code = NA,
    macro_name = NA,
    micro_name = NA
  )%>%
  st_cast("MULTIPOLYGON")%>%
  mutate(
    geoid = paste(paste0(iso3,m49code), macro_code, micro_code, sep="_")
  )%>%
  select(geoid, m49code, iso3, country_name, macro_code, macro_name, everything())%>%
  st_as_sf()%>%
  tibble::remove_rownames()


geomWorld <- geomWorld%>%
  filter(filename != "geomSmallCountries")%>%
  bind_rows(geomSmallCountries)
###
geomGlobal <- ms_simplify(geomWorld,keep=0.05,keep_shapes=TRUE)%>%
  st_make_valid()%>%
  st_wrap_dateline()

st_write(geomGlobal, "countries/data/geom/geomGlobal_simplified.geojson", delete_dsn = T)

groupedGlobal <- geomGlobal%>%
  group_by(filename)

globalList <- group_split(groupedGlobal, .keep = F)
names(globalList) <- pull(group_keys(groupedGlobal))

for(y in seq_along(globalList)){
  assign(x=names(globalList[y]), value = globalList[y][[1]])
  st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/",names(globalList[y]),".geojson"), delete_dsn=T) # set up to replace existing geometries
  rm(list=names(globalList[y]))
}
