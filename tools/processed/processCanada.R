## Do not run this code, it will run automatically when running updateGlobal.R
geomCountry <- sf::st_read("https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/main/geo/hr.geojson")
sf::st_crs(geomCountry) = 3347 # CRS for Canada:  Statistics Canada Lambert
geomCountry = sf::st_transform(geomCountry, crs = 4326)
geomCountry$HR_UID = as.numeric(geomCountry$hruid)


Regions = c()
Regions$abbrev = c("AB","BC","MB","NB","NL","NS","NT","NU","ON","PE","QC","SK","YT")
Regions$longprovince = c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador","Nova Scotia","Northwest Territories","Nunavut","Ontario","Prince Edward Island","Qu\xe9bec","Saskatchewan","Yukon")
Regions  = as.data.frame(Regions)

geomCountry <- dplyr::inner_join(geomCountry,Regions,by = c("region" = "abbrev"))


# UNSD_m49 called by addNewGeom.R

# m49code: numeric country code
# iso3: text country code
# county_name: country name (English)
# macro_code, macro_name: higher region code/name (e.g., US State)
# micro_code, micro_name: data granularity code/name (e.g., US County, the case-aggregate level)
# Use the following if *.geojson includes population data, alternate names, or something else useful

# Make sure LoadCountryName.R references this new file

#geomCountry %>%
#  st_drop_geometry() %>%
#  readr::write_csv(paste0("countries/data/misc", str_to_title("Canada"), ".csv"))
geomCountry <- geomCountry %>%
  dplyr::mutate(country_name = "Canada") %>%
# If country_name doesn't match the format found in updateGeometry/UNSD_m49.xlsx's 'CountryorArea' column, use the following instead:()
# #mutate(match_name = "differentformatname")%>%
# #inner_join(m49, by=c("match_name" = "CountryorArea"))%>%
  dplyr::inner_join(m49, by = c("country_name" = "CountryorArea")) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = region, # corresponding column names from *.geojson
    macro_name = longprovince, # if there isn't a corresponding column, leave it blank or use `micro_name = row_number()` in mutate. You _need_ micro_code or macro_code such that the macro/micro code pair is unique for each row
    micro_code = HR_UID,
    micro_name = name_short
  ) %>%
sf::st_make_valid() %>%
  dplyr::mutate(across(.cols = dplyr::ends_with("code"), .fns = as.character))
assign(paste0("geom", stringr::str_to_title("Canada")), geomCountry)