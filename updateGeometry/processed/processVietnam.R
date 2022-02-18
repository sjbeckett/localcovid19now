# Template

geomCountry <- st_read("countries/data/orig_geom/geomVietnam.geojson") %>%
	st_transform(3406) %>%
	st_make_valid() %>%
	st_transform(4326) %>%
	st_make_valid()
			
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

geomVietnam <- geomCountry%>%
  #mutate(country_name = "Vietnam")%>%
  ## If country_name doesn't match the format found in updateGeometry/UNSD_m49.xlsx's 'CountryorArea' column, use the following instead:
   mutate(match_name = "Viet Nam")%>%
   inner_join(m49, by=c("match_name" = "CountryorArea"))%>%
  mutate(country_name = "Vietnam")%>%
  
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
   # macro_code = , # corresponding column names from *.geojson
   # macro_name = , # if there isn't a corresponding column, leave it blank or use `micro_name = row_number()` in mutate. You _need_ micro_code or macro_code such that the macro/micro code pair is unique for each row
    micro_code = id_1,
    micro_name = name,
  )%>%
  ## Make sure the geometry is valid using sf::st_is_valid(). You may need to project it and project it back to do so (see respecExistingGeom.R for reference)
  st_make_valid()%>% 
  mutate(across(.cols=ends_with("code"),.fns=as.character))

## store copy of data in /temp_geom/. This isn't necessary if running updateGlobal.R
# st_write(geomCountry, "countries/data/temp_geom/geomCountry.geojson")