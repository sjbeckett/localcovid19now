# Small Countries
## Changed to add micro_name and micro_code that are the same as country code and name
geomSmallCountries <- st_read("countries/data/orig_geom/WB_countries_Admin0_lowres.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/popSmallCountries.csv")) {
  geomSmallCountries %>%
    st_drop_geometry() %>%
    write_csv("countries/data/popSmallCountries.csv")
  cat("\npopSmallCountries.csv Created\n")
}else{
  cat("\npopSmallCountries.csv Already Exists\n")
}

geomSmallCountries <- geomSmallCountries %>%
  select(
    m49code = ISO_N3,
    iso3 = ISO_A3,
    country_name = NAME_EN
  )%>%
  mutate(
    micro_name = country_name,
    micro_code = iso3
  )%>%
  filter(iso3!=-99)

geomFiji <- geomSmallCountries %>%
  filter(iso3 == "FJI") %>% # Fiji
  st_cast("POLYGON") %>%
  st_wrap_dateline() %>%
  group_by(m49code, iso3, country_name) %>%
  summarise() %>%
  ungroup()

geomSmallCountries <- geomSmallCountries[which(st_is_valid(geomSmallCountries, reason = T) == "Valid Geometry"), ] %>% # Russia, USA, and Japan are accounted for in other data sets, so we can afford to loose them
  bind_rows(geomFiji) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

rm(geomFiji)
