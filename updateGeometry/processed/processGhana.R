# Ghana

geomGhana <- st_read("countries/data/orig_geom/geomGhana.geojson")

# These are marco regions because there are subdivisions, not sure if we'll find a better data set with micro regions. Load Ghana reflects this
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

## Unused when run with updateGlobal 
# st_write(geomGhana, "countries/data/temp_geom/geomGhana.geojson")