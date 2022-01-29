source("librariesMinimal.R")
library(rmapshaper) # for the ms_simplify function - simplifies polygons.

# Open the most recent WorldPreSimplified geojson. Manually modify if this is not the most up-to-date file.
zipfiles <- file.info(list.files("updateGeometry/WorldPreSimplified",full.names = T))
zipfile <- row.names(filter(zipfiles, ctime == max(ctime)))
temp <- unzip(zipfile = zipfile, exdir = tempdir())
geomWorld <- st_read(temp)
unlink(temp)

### Instructions
## Add original geomCountry.geojson to countries/data/orig_geom
## Create new script "updateGeometry/toProcess/processCounty.R" that matches processTemplate's format.
## Run updateGlobal.R

source("updateGeometry/addNewGeom.R")
newWorld <- addNewGeoms()

# If new geometries are added in newWorld, remove the old ones from geomWorld and add the new ones
if (!is.null(newWorld)) { # If no new geometries are added, newWorld will be NULL
  updatedFiles <- unique(newWorld$filename)
  geomWorld <- geomWorld %>%
    filter(!filename %in% updatedFiles) %>%
    bind_rows(
      newWorld
    )
  # Write a new geomWorld_presimp that includes the updated geometry
  st_write(geomWorld, "geomWorld_presimp.geojson")
  nowtime <- round(difftime(now(tzone="UTC"),ymd_hms("1970-01-01 00:00:00"), tz="UTC", units="mins"))
  # the presimplified world geometry is too big for github, so put it in a zip file and remove the geojson
  zip(zipfile = paste0("countries/data/WorldPreSimp",nowtime,".zip"), "geomWorld_presimp.geojson")
  file.remove("geomWorld_presimp.geojson")
  rm(newWorld)
}

# Simplify the world
geomGlobal <- ms_simplify(geomWorld, keep = 0.05, explode = T, keep_shapes = TRUE) %>%
  group_by(geoid)%>%
  st_make_valid()%>%
  # There are some duplicated geoids due to geomSmallCountries, but these are filtered out in the LoadCountry stage
  summarise(
    m49code = first(m49code),
    iso3 = first(iso3),
    country_name = first(country_name),
    macro_code = first(macro_code),
    macro_name = first(macro_name),
    micro_code = first(micro_code),
    micro_name = first(micro_name),
    filename = first(filename)
  )%>%
  ungroup() %>%
  st_wrap_dateline() # removes horizontal bar when Fiji crosses the dateline

st_write(geomGlobal, "countries/data/geom/geomGlobal_simplified.geojson", delete_dsn = T)

# group geomGlobal by filename
groupedGlobal <- geomGlobal %>%
  group_by(filename)
# create a list of each group
globalList <- group_split(groupedGlobal, .keep = F) # remove grouping property
# name each stage of the list with the filename (group_key = filename)
names(globalList) <- pull(group_keys(groupedGlobal))

# Write each geometry to the data/geom folder and save as filename.geojson
for (y in seq_along(globalList)) {
  assign(x = names(globalList[y]), value = globalList[y][[1]])
  st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/", names(globalList[y]), ".geojson"), delete_dsn = T) # set up to replace existing geometries. Would like to add a check to see if files are the same. before overwriting

  rm(list = names(globalList[y]))
}


# file.info(list.files("countries/data/geom/",full.names = T))%>%
#   write.csv("geometry_compare.csv")
