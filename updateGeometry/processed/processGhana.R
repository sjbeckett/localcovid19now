# Ghana

geomGhana <- st_read("countries/data/orig_geom/geomGhana.geojson")

geomGhana$macro_code = c("SV","BO", "AF", "BE", "AA", "OT", "WP", "WN", "UW", "UE", "EP", "CP", "AH", "NE", "NP", "TV")

geomGhana <- geomGhana%>%
  mutate(country_name = "Ghana")%>%
  inner_join(m49, by=c("country_name" = "CountryorArea"))%>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code,
    macro_name = REGION
  )%>%
  st_make_valid()%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))
# 
# st_write(geomGhana, "countries/data/temp_geom/geomGhana.geojson")