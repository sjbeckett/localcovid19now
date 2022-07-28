# Small Countries
## Changed to add micro_name and micro_code that are the same as country code and name
geomSmallCountries <- st_read("countries/data/orig_geom/WB_countries_Admin0_lowres.geojson")

## Only create csv if it doesn't exist.
if (!file.exists("countries/data/popSmallCountries.csv")) {
  geomSmallCountries %>%
    st_drop_geometry() %>%
    write_csv("countries/data/popSmallCountries.csv")
  cat("\npopSmallCountries.csv Created\n")
} else {
  cat("\npopSmallCountries.csv Already Exists\n")
}

geomSmallCountries <- geomSmallCountries %>%
  mutate(
    iso3 = case_when(
      ISO_A3 == "UMI" ~ "USA",
      ISO_A3 == -99 ~ ISO_A3_EH,
      TRUE ~ ISO_A3
    ),
    country_name = if_else(
      ISO_A3 == "UMI", "United States",
      NAME_EN
    ),
    macro_code = if_else(
      ISO_A3 == "UMI", "UM",
      NA_character_
    ),
    m49code = if_else(
      ISO_A3 == "UMI", "840",
      ISO_N3
    )
  ) %>%
  select(
    m49code,
    iso3,
    country_name,
    macro_code
  ) %>%
  filter(iso3 != -99)

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
