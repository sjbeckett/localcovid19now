# Small Countries
## Changed to add micro_name and micro_code that are the same as country code and name
# Europe
geomEurope <- sf::st_read(here::here("updateGeometry/geomEurope.geojson"))
namesEurope <- vroom::vroom(here::here("updateGeometry/namesEurope.csv"), show_col_types = FALSE, progress = FALSE)

temp <- unzip(zipfile = here::here("updateGeometry/ne_10m_admin_0_countries_lakes.zip"), exdir = tempdir())

worldBorders <- sf::st_read(temp[which(stringr::str_detect(temp, ".shp$"))])%>%
  sf::st_drop_geometry()%>%
  dplyr::mutate(
    ISO_A3 = dplyr::case_when(
      ISO_A3 == -99 & ISO_A3_EH == -99 ~ ADM0_A3,
      ISO_A3 == -99 ~ ISO_A3_EH,
      TRUE ~ ISO_A3
    ),
    ISO_N3 = dplyr::case_when(
      ISO_N3 == -99 ~ ISO_N3_EH,
      TRUE ~ ISO_N3
    )
  )
unlink(temp)

geomEurope <- geomEurope %>%
  dplyr::inner_join(namesEurope, by = "UID") %>%
  dplyr::mutate(
    Region = dplyr::case_when(
      Region == "Faroe" ~ "Faeroe Islands",
      TRUE ~ Region
    ),
    CountryName = dplyr::case_when(
      CountryName == "Russian Fed." ~ "Russian Federation",
      CountryName == "Holy See" ~ "Vatican",
      # CountryName == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
      Region %in% worldBorders$NAME_LONG ~ Region,
      TRUE ~ CountryName
    )
  ) %>%
  dplyr::left_join(worldBorders, by = c("CountryName"="NAME_LONG"))%>%
  dplyr::mutate(
    CountryName = dplyr::case_when(
      CountryName == "Czech Republic" ~ "Czechia",
      CountryName == "Vatican" ~ "Holy See",
      TRUE ~ CountryName
  )
  )%>%
  dplyr::select(
    m49code = ISO_N3,
    iso3 = ISO_A3,
    country_name = CountryName,
    micro_code = UID,
    micro_name = Region
  ) %>%
  dplyr::mutate(
    iso3 = dplyr::if_else(micro_name == "Svalbard and Jan Mayen Islands", "SJM", iso3)
  )%>%
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("code"), .fns = as.character))
## Make Europe valid
geomEurope <- geomEurope %>%
  sf::st_transform("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>% # ESRI 102014: Europe Lambert Conformal Conic
  sf::st_make_valid() %>%
  sf::st_transform(4326)

rm(namesEurope)
rm(worldBorders)

