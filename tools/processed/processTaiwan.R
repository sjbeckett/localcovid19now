# Taiwan

geomTaiwan <- st_read("countries/data/orig_geom/geomTaiwan.geojson")

TRegions = vroom("countries/data/TaiwanNames.csv", .name_repair = make.names)

geomTaiwan <- geomTaiwan  %>%
  filter(!COUNTYSN %in% c("10011001", "10006003"))%>%
  group_by(COUNTYNAME)%>%
  summarise()%>%
  ungroup()%>%
  mutate(
    country_name = "Taiwan",
    m49code = 158, # Using ISO3166 code bc Taiwan is not in the M49 list
    iso3 = "TWN"
  ) %>%
  full_join(TRegions, by=c("COUNTYNAME"="Chinese.name"))%>%
  select(
    m49code,
    iso3,
    country_name,
    micro_code,
    micro_name = COUNTYNAME
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

## Unused when run with updateGlobal 
# geomTaiwan %>%
#   st_write("countries/data/temp_geom/geomTaiwan.geojson")
## UNSD_m49 called by addNewGeom.R

### m49code: numeric country code
### iso3: text country code
### county_name: country name (English)
### macro_code, macro_name: higher region code/name (e.g., US State)
### micro_code, micro_name: data granularity code/name (e.g., US County, the case-aggregate level)