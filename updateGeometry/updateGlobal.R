source("librariesMinimal.R")
library(rmapshaper) # for the ms_simplify function - simplifies polygons.
temp <- unzip(zipfile = "countries/data/WorldPreSimp.zip", exdir = tempdir())
geomWorld <- st_read(temp)
unlink(temp)

### Instructions
## Add original *.geojson to countries/data/orig_geom
## Create new script "updateGeometry/toProcess/processCounty.R" that matches processGhana's format.
## Run updateGlobal.R

source("updateGeometry/addNewGeom.R")
newWorld <- addNewGeoms()

if (!is.null(newWorld)) {
  updatedFiles <- unique(newWorld$filename)
  geomWorld <- geomWorld %>%
    filter(!filename %in% updatedFiles) %>%
    bind_rows(
      newWorld
    )
}


geomGlobal <- ms_simplify(geomWorld, keep = 0.05, keep_shapes = TRUE) %>%
  st_make_valid() %>%
  st_wrap_dateline()

st_write(geomGlobal, "countries/data/geom/geomGlobal_simplified.geojson", delete_dsn = T)

groupedGlobal <- geomGlobal %>%
  group_by(filename)

globalList <- group_split(groupedGlobal, .keep = F)
names(globalList) <- pull(group_keys(groupedGlobal))

for (y in seq_along(globalList)) {
  assign(x = names(globalList[y]), value = globalList[y][[1]])
  st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/", names(globalList[y]), ".geojson"), delete_dsn = T) # set up to replace existing geometries

  rm(list = names(globalList[y]))
}