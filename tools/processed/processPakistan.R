## Do not run this code, it will run automatically when running updateGlobal.R
geomCountry <- sf::st_read("https://github.com/wmgeolab/geoBoundaries/raw/dad1bd2f93aa5d782a109e45057cfcc8c520038c/releaseData/gbOpen/PAK/ADM1/geoBoundaries-PAK-ADM1_simplified.geojson")
geomCountry$shapeName[which(geomCountry$shapeName == "Azad Kashmir")] <- "Azad Jammu and Kashmir"
geomCountry$shapeName[which(geomCountry$shapeName == "Islamabad Capital Territory")] <- "Islamabad"
geomCountry$micro_name <- geomCountry$shapeName
geomCountry$micro_code <- substr(geomCountry$shapeISO, 4, 5)
# UNSD_m49 called by addNewGeom.R

# m49code: numeric country code
# iso3: text country code
# county_name: country name (English)
# macro_code, macro_name: higher region code/name (e.g., US State)
# micro_code, micro_name: data granularity code/name (e.g., US County, the case-aggregate level)
# Use the following if *.geojson includes population data, alternate names, or something else useful

# Make sure LoadCountryName.R references this new file

# assign(x=paste0("misc", stringr::str_to_title("Pakistan")), value=sf::st_drop_geometry(geomCountry))
# cat(exists(paste0("misc", stringr::str_to_title("Pakistan"))))
# usethis::use_data(name = paste0("misc", stringr::str_to_title("Pakistan")), internal = FALSE)

purrr::walk2(list(sf::st_drop_geometry(geomCountry)), paste0("misc", stringr::str_to_title("Pakistan")), function(obj, name) {
  assign(name, obj)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
})


geomCountry <- geomCountry %>%
  dplyr::mutate(country_name = "Pakistan") %>%
  # If country_name doesn't match the format found in updateGeometry/UNSD_m49.xlsx's 'CountryorArea' column, use the following instead:()
  # #mutate(match_name = "differentformatname")%>%
  # #inner_join(m49, by=c("match_name" = "CountryorArea"))%>%
  dplyr::inner_join(m49, by = c("country_name" = "CountryorArea")) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    # macro_code = "macro_code", # corresponding column names from *.geojson
    # macro_name = "macro_name", # if there isn't a corresponding column, leave it blank or use `micro_name = row_number()` in mutate. You _need_ micro_code or macro_code such that the macro/micro code pair is unique for each row
    micro_code,
    micro_name
  ) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))
assign(paste0("geom", stringr::str_to_title("Pakistan")), value = geomCountry)
