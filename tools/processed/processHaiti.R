## Do not run this code, it will run automatically when running updateGlobal.R
geomCountry <- sf::st_read("https://github.com/samateja/D3topoJson/raw/master/haiti.json")
# UNSD_m49 called by addNewGeom.R
geomCountry <- sf::st_set_crs(geomCountry,"EPSG:4326")


# m49code: numeric country code
# iso3: text country code
# county_name: country name (English)
# macro_code, macro_name: higher region code/name (e.g., US State)
# micro_code, micro_name: data granularity code/name (e.g., US County, the case-aggregate level)
# Use the following if *.geojson includes population data, alternate names, or something else useful

# Make sure LoadCountryName.R references this new file


CountryName <- "Haiti" # Make variable with country name to fill in below. Be sure to use camelcase (e.g., "UnitedKingdom")

purrr::walk2(sf::st_drop_geometry(geomCountry), CountryName, function(obj, name) {
    assign(name, obj)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
})

geomCountry <- geomCountry %>%
  dplyr::mutate(country_name = CountryName) %>%
# If country_name doesn't match the format found in updateGeometry/UNSD_m49.xlsx's 'CountryorArea' column, use the following instead:()
# #mutate(match_name = "differentformatname")%>%
# #inner_join(m49, by=c("match_name" = "CountryorArea"))%>%
dplyr::inner_join(m49, by = c("country_name" = "CountryorArea")) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = , # corresponding column names from *.geojson
    macro_name = , # if there isn't a corresponding column, leave it blank or use `macro_name = row_number()` in mutate. You _need_ micro_code or macro_code such that the macro/micro code pair is unique for each row
    micro_code = iso_3166_2,
    micro_name = name
  ) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(across(.cols = dplyr::ends_with("code"), .fns = as.character))
assign(paste0("geom", CountryName), geomCountry)