#' Update Global Geometries
#'
#' @return
#' @export
#'
#' @examples
#' updateGlobal()
updateGlobal <- function(){
  

# Requires rmapshaper for the ms_simplify function - simplifies polygons.
  rlang::check_installed(c("rmapshapper"), reason = "to use `updateGlobal()`")

# Open the most recent WorldPreSimplified geojson. Manually modify if this is not the most up-to-date file.
## Because zipfile named with seconds post epoch, `max(dir("updateGeometry/WorldPreSimplified"))` gives most recent version. Using ctime property potentially not reliable bc file creation with git.
zipfiles <- list.files(here::here("tools/WorldPreSimplified"))

temp <- utils::unzip(zipfile = here::here("tools/WorldPreSimplified", max(zipfiles)), exdir = tempdir())

geomWorld <- sf::st_read(temp)
unlink(temp)

### Instructions
## Add original geomCountry.geojson to countries/data/orig_geom
## Create new script "updateGeometry/toProcess/processCounty.R" that matches processTemplate's format.
## Run updateGlobal.R

# source("updateGeometry/addNewGeom.R")
newWorld <- addNewGeoms()

# If new geometries are added in newWorld, remove the old ones from geomWorld and add the new ones
if (!is.null(newWorld)) { # If no new geometries are added, newWorld will be NULL
  updatedFiles <- unique(newWorld$filename)
  geomWorld <- geomWorld %>%
    dplyr::filter(!filename %in% updatedFiles) %>%
    dplyr::bind_rows(
      newWorld
    )
  
  # Remove geomSmallCountries and geomEurope entries that are supplied by other data sets
# source("updateGeometry/remSurplus.R")
  geomWorld <- remSurplus(geomWorld, c("geomEurope","geomSmallCountries"))

  # Write a new geomWorld_presimp that includes the updated geometry
  sf::st_write(geomWorld, "geomWorld_presimp.geojson")

  nowtime <- round(difftime(lubridate::now(tzone = "UTC"), hms::ymd_hms("1970-01-01 00:00:00"), tz = "UTC", units = "mins"))
  # the presimplified world geometry is too big for github, so put it in a zip file and remove the geojson
  zip(zipfile = paste0("tools/WorldPreSimplified/WorldPreSimp", nowtime, ".zip"), "geomWorld_presimp.geojson")
  file.remove("geomWorld_presimp.geojson")
  rm(newWorld)
}

# Simplify the world
geomGlobal <- rmapshaper::ms_simplify(geomWorld, keep = 0.05, explode = T, keep_shapes = TRUE) %>%
  dplyr::group_by(geoid) %>%
  sf::st_make_valid() %>%
  # There are some duplicated geoids due to geomSmallCountries, but these are filtered out in the LoadCountry stage
  dplyr::summarise(
    m49code = dplyr::first(m49code),
    iso3 = dplyr::first(iso3),
    country_name = dplyr::first(country_name),
    macro_code = dplyr::first(macro_code),
    macro_name = dplyr::first(macro_name),
    micro_code = dplyr::first(micro_code),
    micro_name = dplyr::first(micro_name),
    filename = dplyr::first(filename)
  ) %>%
  dplyr::ungroup() %>%
  sf::st_wrap_dateline() # removes horizontal bar when Fiji crosses the dateline

# ## Add mapshapper clean code here 

# Saving entire clean geometry, not sure which method we want to use. It currently isn't called anywhere in the package, but it could be later if we wanted to only join to geometry once.
sf::st_write(geomGlobal, "tools/geomGlobal_simplified.geojson", delete_dsn = T)
usethis::use_data(geomGlobal, overwrite = T)

# group geomGlobal by filename
groupedGlobal <- geomGlobal %>%
  dplyr::group_by(filename)
# create a list of each group
globalList <- dplyr::group_split(groupedGlobal, .keep = F) # remove grouping property
# name each stage of the list with the filename (group_key = filename)
names(globalList) <- dplyr::pull(dplyr::group_keys(groupedGlobal))

# Save each geometry as filename.Rda for the package to use
for (y in seq_along(globalList)) {
  cat("\n")
  # if (file.exists(here::here("data", paste0(names(globalList[y]), ".rda"))) & !names(globalList[y]) %in% updatedFiles) {
  
  # If the relevant geometry is already in the pacakge, check if it's different than the new one
    if (exists(names(globalList[y])) & !names(globalList[y]) %in% updatedFiles) {
    # Bring in existing geometry
    data(list=names(globalList[y]), envir = environment())
    assign(x="existing",get(names(globalList[y])))
    
    # existing <- sf::st_read(paste0("countries/data/geom/", names(globalList[y]), ".geojson"), quiet = T)
    # Isolate the new geometry
    newgeom <- globalList[y][[1]]

    # If all geometry of the new and existing files AND all the dataframe values of new and existing are the same, do not overwrite the existing file
    if (!all(purrr::map_lgl(
      1:nrow(newgeom),
      ~ sf::st_geometry(existing)[.x] == sf::st_geometry(newgeom)[.x]
    ), na.rm = T) & !all(sf::st_drop_geometry(existing) == sf::st_drop_geometry(newgeom), na.rm = T)
    ) {
      # If they aren't the same, overwrite the existing file
      cat("overwrite existing geometry: ", names(globalList[y]), "\n")
      assign(x = names(globalList[y]), value = globalList[y][[1]])
      usethis::use_data(get(names(globalList[y])), overwrite = T) # Use data for package
      # sf::st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/", names(globalList[y]), ".geojson"), delete_dsn = T)
      rm(list = names(globalList[y]))
    } else {
      cat("geometries (", y, ") are the same\n")
    }
  } else {
    # If the geometry is part of the newly updated geometries (updatedFiles), over(write) without checking as changes have been made and it may not be the same size as the existing geometry, which would throw an error in the similarity check
    cat("write new geometry: ", names(globalList[y]), "\n")
    assign(x = names(globalList[y]), value = globalList[y][[1]])
    usethis::use_data(get(names(globalList[y])), overwrite = T) # Use data for package
    # sf::st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/", names(globalList[y]), ".geojson"), delete_dsn = T)
    rm(list = names(globalList[y]))
  }
}
rm(existing, newgeom)

}