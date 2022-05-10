# Ecuador

geomEcuador <- st_read("countries/data/orig_geom/geomEcuador.geojson")
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
# readr::write_csv("countries/data/miscCountry.csv")}

geomEcuador <- geomEcuador%>%
  mutate(country_name = "Ecuador")%>%
  ## If country_name doesn't match the format found in updateGeometry/UNSD_m49.xlsx's 'CountryorArea' column, use the following instead:
  # mutate(match_name = "differentformatname")%>%
  # inner_join(m49, by=c("match_name" = "CountryorArea"))%>%
  inner_join(m49, by=c("country_name" = "CountryorArea"))%>%
  
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = ADM1_PCODE,
    macro_name = ADM1_ES,
	micro_code = ADM2_PCODE,
    micro_name = ADM2_ES,
  )%>%
  st_make_valid()%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

## store copy of data in /temp_geom/. This isn't necessary if running updateGlobal.R
# st_write(geomCountry, "countries/data/temp_geom/geomCountry.geojson")