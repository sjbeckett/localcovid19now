source("librariesMinimal.R")
library(rmapshaper) # for the ms_simplify function - simplifies polygons.
here::i_am("updateGeometry/updateGlobal.R")

# Open the most recent WorldPreSimplified geojson. Manually modify if this is not the most up-to-date file.
## Because zipfile named with seconds post epoch, `max(dir("updateGeometry/WorldPreSimplified"))` gives most recent version. Using ctime property potentially not reliable bc file creation with git.
zipfiles <- list.files(here::here("updateGeometry/WorldPreSimplified"))

temp <- unzip(zipfile = here::here("updateGeometry/WorldPreSimplified", max(zipfiles)), exdir = tempdir())

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


  nowtime <- round(difftime(now(tzone = "UTC"), ymd_hms("1970-01-01 00:00:00"), tz = "UTC", units = "mins"))
  # the presimplified world geometry is too big for github, so put it in a zip file and remove the geojson
  zip(zipfile = paste0("updateGeometry/WorldPreSimplified/WorldPreSimp", nowtime, ".zip"), "geomWorld_presimp.geojson")
  file.remove("geomWorld_presimp.geojson")
  rm(newWorld)
}

# Simplify the world
geomGlobal <- ms_simplify(geomWorld, keep = 0.05, explode = T, keep_shapes = TRUE) %>%
  group_by(geoid) %>%
  st_make_valid() %>%
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
  ) %>%
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
  cat("\n")
  # Bring in the existing geometry if it exists
  if (file.exists(paste0("countries/data/geom/", names(globalList[y]), ".geojson"))) {
    existing <- st_read(paste0("countries/data/geom/", names(globalList[y]), ".geojson"), quiet = T)
    # Isolate at the new one
    newgeom <- globalList[y][[1]]

    # If all geometry of the new and existing files AND all the dataframe values of new and existing are the same, do not overwrite the existing file
    if (!all(map_lgl(
      1:nrow(newgeom),
      ~ st_geometry(existing)[.x] == st_geometry(newgeom)[.x]
    ), na.rm = T) & !all(st_drop_geometry(existing) == st_drop_geometry(newgeom), na.rm = T)
    ) {
      cat("overwrite existing geometry: ", names(globalList[y]), "\n")
      assign(x = names(globalList[y]), value = globalList[y][[1]])
      st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/", names(globalList[y]), ".geojson"), delete_dsn = T)
      rm(list = names(globalList[y]))
    } else {
      cat("geometries (", y, ") are the same\n")
    }
  } else {
    cat("write new geometry: ", names(globalList[y]), "\n")
    assign(x = names(globalList[y]), value = globalList[y][[1]])
    st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/", names(globalList[y]), ".geojson"), delete_dsn = T)
    rm(list = names(globalList[y]))
  }
}
rm(existing, newgeom)