# Small Countries
## Changed to add micro_name and micro_code that are the same as country code and name
# Europe

geomEurope <- st_read("countries/data/orig_geom/geomEurope.geojson")
namesEurope <- vroom::vroom("countries/data/namesEurope.csv")

temp <- unzip(zipfile = here::here("updateGeometry/ne_10m_admin_0_countries_lakes.zip"), exdir = tempdir())

worldBorders <- st_read(temp[which(str_detect(temp, ".shp$"))])%>%
  st_drop_geometry()%>%
  mutate(
    ISO_A3 = case_when(
      ISO_A3 == -99 & ISO_A3_EH == -99 ~ ADM0_A3,
      ISO_A3 == -99 ~ ISO_A3_EH,
      TRUE ~ ISO_A3
    ),
    ISO_N3 = case_when(
      ISO_N3 == -99 ~ ISO_N3_EH,
      TRUE ~ ISO_A3
    )
  )
unlink(temp)

geomEurope <- geomEurope %>%
  inner_join(namesEurope, by = "UID") %>%
  mutate(
    Region = case_when(
      Region == "Faroe" ~ "Faeroe Islands",
      TRUE ~ Region
    ),
    CountryName = case_when(
      CountryName == "Russian Fed." ~ "Russian Federation",
      CountryName == "Holy See" ~ "Vatican",
      # CountryName == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
      Region %in% worldBorders$NAME_LONG ~ Region,
      TRUE ~ CountryName
    )
  ) %>%
  left_join(worldBorders, by = c("CountryName"="NAME_LONG"))%>%
  mutate(
    CountryName = case_when(
      CountryName == "Czech Republic" ~ "Czechia",
      CountryName == "Vatican" ~ "Holy See",
      TRUE ~ CountryName
  )
  )%>%
  select(
    m49code = ISO_N3,
    iso3 = ISO_A3,
    country_name = CountryName,
    micro_code = UID,
    micro_name = Region
  ) %>%
  mutate(
    iso3 = if_else(micro_name == "Svalbard and Jan Mayen Islands", "SJM", iso3)
  )%>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))
## Make Europe valid
geomEurope <- geomEurope %>%
  st_transform("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>% # ESRI 102014: Europe Lambert Conformal Conic
  st_make_valid() %>%
  st_transform(4326)

rm(namesEurope)
rm(worldBorders)

