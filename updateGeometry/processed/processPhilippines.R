# Philippines

geomPhilippines <- st_read("countries/data/orig_geom/geomPhilippines.geojson")
namesPhilippines <- vroom("countries/data/namesPhilippines.csv") %>%
  select(starts_with("ADM2"), starts_with("ADM1")) %>%
  distinct()

geomPhilippines <- geomPhilippines %>%
  mutate(
    country_name = "Philippines"
  ) %>%
  left_join(namesPhilippines) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  mutate(
    macro_code = str_sub(ADM1_PCODE, 3,4),
    micro_code = str_sub(ADM2_PCODE, 4,6)
  )%>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code,
    macro_name = ADM1_EN,
    micro_code,
    micro_name = ADM2_EN
  ) %>%
  group_by(m49code, iso3,  country_name, macro_code,  macro_name, micro_name)%>%
  summarise(
    micro_code = first(micro_code)
  )%>%
  ungroup()%>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

## Unused when run with updateGlobal 
# geomPhilippines %>%
  # st_write("countries/data/temp_geom/geomPhilippines.geojson")
# rm(geomPhilippines)
rm(namesPhilippines)
## UNSD_m49 called by addNewGeom.R
