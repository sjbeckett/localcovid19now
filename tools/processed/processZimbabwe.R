# Zimbabwe

geomZimbabwe <- st_read("countries/data/orig_geom/geomZimbabwe.geojson")
## UNSD_m49 called by addNewGeom.R

### m49code: numeric country code
### iso3: text country code
### county_name: country name (English)
### macro_code, macro_name: higher region code/name (e.g., US State)
### micro_code, micro_name: data granularity code/name (e.g., US County, the case-aggregate level)

## Use the following if *.geojson includes population data, alternate names, or something else useful
#if(!file.exists("countries/data/miscCountry.csv")){
## Make sure LoadCountry.R references this new file.
#geomCountry%>%
#  st_drop_geometry()%>%
#  readr::write_csv("countries/data/miscCountry.csv")}

geomZimbabwe <- geomZimbabwe%>%
  mutate(country_name = "Zimbabwe")%>%
  ## If country_name doesn't match the format found in updateGeometry/UNSD_m49.xlsx's 'CountryorArea' column, use the following instead:
  # mutate(match_name = "differentformatname")%>%
  # inner_join(m49, by=c("match_name" = "CountryorArea"))%>%
  inner_join(m49, by=c("country_name" = "CountryorArea"))%>%
  
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    #macro_code = , # corresponding column names from *.geojson
    #macro_name = , # if there isn't a corresponding column, leave it blank or use `micro_name = row_number()` in mutate. You _need_ micro_code or macro_code such that the macro/micro code pair is unique for each row
    micro_code = provincepc,
    micro_name = province,
  )%>%
  ## Make sure the geometry is valid using sf::st_is_valid(). You may need to project it and project it back to do so (see respecExistingGeom.R for reference)
  st_make_valid()%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

## store copy of data in /temp_geom/. This isn't necessary if running updateGlobal.R
# st_write(geomCountry, "countries/data/temp_geom/geomCountry.geojson")