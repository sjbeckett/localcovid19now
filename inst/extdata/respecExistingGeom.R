source("librariesMinimal.R")
library(readr)

# Fields will be as follows:
## GEOID = ISO3m49_micro_macro
## m49code
## iso3
## county_name
## macro_code
## macro_name
## micro_code
## micro_name
## geometry

respecEnv <- new.env()

## Load in the m49 codes for countries
m49 <- readxl::read_xlsx("updateGeometry/UNSD_m49.xlsx")
m49 <- m49 %>%
  rename_with(
    .fn = \(x) str_replace_all(x, "[\\s/-]", "")
  )

# Afghanistan

respecEnv$geomAfghanistan <- sf::st_read("countries/data/orig_geom/geomAfghanistan.geojson")

respecEnv$geomAfghanistan <- respecEnv$geomAfghanistan %>%
  dplyr::mutate(
    country_name = "Afghanistan",
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = id,
    micro_name = name
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Algeria

respecEnv$geomAlgeria <- sf::st_read("countries/data/orig_geom/geomAlgeria.geojson")

respecEnv$geomAlgeria <- respecEnv$geomAlgeria %>%
  dplyr::mutate(
    country_name = "Algeria",
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = ID,
    micro_name = NAME
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Argentina

respecEnv$geomArgentina <- sf::st_read("countries/data/orig_geom/geomArgentina.geojson")
## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscArgentina.csv")) {
  respecEnv$geomArgentina %>%
    st_drop_geometry() %>%
    write_csv("countries/data/miscArgentina.csv")
  cat("\nmiscArgentina.csv Created\n")
}else{
  cat("\nmiscArgentina.csv Already Exists\n")
}

respecEnv$geomArgentina <- respecEnv$geomArgentina %>%
  dplyr::mutate(
    country_name = "Argentina",
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = ID_1,
    micro_name = NAME_1
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Australia

respecEnv$geomAustralia <- sf::st_read("countries/data/orig_geom/geomAustralia.geojson")

respecEnv$geomAustralia <- respecEnv$geomAustralia %>%
  dplyr::mutate(
    country_name = "Australia",
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = STATE_CODE,
    micro_name = STATE_NAME
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Austria

respecEnv$geomAustria <- sf::st_read("countries/data/orig_geom/geomAustria.geojson")

respecEnv$geomAustria <- respecEnv$geomAustria %>%
  mutate(
    country_name = "Austria",
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = iso,
    micro_name = name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Belgium

respecEnv$geomBelgium <- sf::st_read("countries/data/orig_geom/geomBelgium.geojson")

respecEnv$geomBelgium <- respecEnv$geomBelgium %>%
  dplyr::mutate(
    country_name = "Belgium",
    micro_code = row_number() # I'm assigning IDs based on row number, but we could go back and do country subdivision ISOs
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = NameDUT
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Brazil

respecEnv$geomBrazil <- sf::st_read("countries/data/orig_geom/geomBrazil.geojson")

respecEnv$geomBrazil <- respecEnv$geomBrazil %>%
  dplyr::mutate(
    country_name = "Brazil"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = regiao_id,
    micro_code = sigla,
    micro_name = nome
  ) %>%
  sf::st_make_valid() %>% # Brazil has some invalidity
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Canada

respecEnv$geomCanada <- sf::st_read("countries/data/orig_geom/geomCanada.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscCanada.csv")) {
  respecEnv$geomCanada %>%
    sf::st_drop_geometry() %>%
    write_csv("countries/data/miscCanada.csv")
  cat("\nmiscCanada.csv Created\n")
}else{
  cat("\nmiscCanada.csv Already Exists\n")
}

respecEnv$geomCanada <- respecEnv$geomCanada %>%
  dplyr::mutate(
    country_name = "Canada"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = Province,
    micro_code = HR_UID,
    micro_name = ENGNAME
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))
# Make canada valid
respecEnv$geomCanada <- respecEnv$geomCanada %>%
  sf::st_transform(3347) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# Chile

respecEnv$geomChile <- sf::st_read("countries/data/orig_geom/geomChile.geojson")

if (!file.exists("countries/data/miscChile.csv")) {
  respecEnv$geomChile %>%
    sf::st_drop_geometry() %>%
    write_csv("countries/data/miscChile.csv")
  cat("\nmiscChile.csv Created\n")
}else{
  cat("\nmiscChile.csv Already Exists\n")
}

respecEnv$geomChile <- respecEnv$geomChile %>%
  dplyr::mutate(
    country_name = "Chile"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = codregion,
    micro_name = matchName
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))
# Make Chile valid
respecEnv$geomChile <- respecEnv$geomChile %>%
  sf::st_transform("+proj=utm +zone=23 +south +ellps=aust_SA +units=m +no_defs") %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# China

respecEnv$geomChina <- sf::st_read("countries/data/orig_geom/geomChina.geojson")

## Only create csv if it doesn't exist.
if (!file.exists("countries/data/miscChina.csv")) {
  respecEnv$geomChina %>%
    sf::st_drop_geometry() %>%
    write_csv("countries/data/miscChina.csv")
  cat("\nmiscChina.csv Created\n")
}else{
  cat("\nmiscChina.csv Already Exists\n")
}

respecEnv$geomChina <- respecEnv$geomChina %>%
  dplyr::mutate(
    country_name = "China"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = ISO,
    micro_name = NAME_1
  ) %>%
  dplyr::filter(micro_code != "CN-71") %>% # Remove Taiwan because we have higher resolution in geomTaiwan
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Colombia

respecEnv$geomColombia <- sf::st_read("countries/data/orig_geom/geomColombia.geojson")

respecEnv$geomColombia <- respecEnv$geomColombia %>%
  dplyr::mutate(
    country_name = "Colombia",
    NOMBRE_DPT = str_to_title(NOMBRE_DPT)
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = DPTO,
    micro_name = NOMBRE_DPT
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Cuba

respecEnv$geomCuba <- sf::st_read("countries/data/orig_geom/geomCuba.geojson")

respecEnv$geomCuba <- respecEnv$geomCuba %>%
  dplyr::mutate(
    country_name = "Cuba",
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = province_id,
    micro_name = province
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character)) %>%
  dplyr::filter(micro_code != "unk")

## Make Cuba valid
respecEnv$geomCuba <- respecEnv$geomCuba %>%
  sf::st_transform(32617) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

# Czechia

respecEnv$geomCzechia <- sf::st_read("countries/data/orig_geom/geomCzechia.geojson")

namesCzechia <- vroom::vroom("countries/data/czech_pop.csv") %>%
  dplyr::select(-Population)
## Load codes for Local Admin Units (LAU)
respecEnv$geomCzechia <- respecEnv$geomCzechia %>%
  dplyr::full_join(namesCzechia, by = c("name" = "District")) %>%
  dplyr::mutate(
    country_name = "Czechia",
    macro_code = str_sub(Code, 1, 5) # NUTS 3 level code
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code,
    micro_code = Code,
    micro_name = name
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

rm(namesCzechia)

# Denmark

respecEnv$geomDenmark <- sf::st_read("countries/data/orig_geom/geomDenmark.geojson")

respecEnv$geomDenmark <- respecEnv$geomDenmark %>%
  sf::st_zm() %>%
  dplyr::group_by(name) %>%
  dplyr::summarise() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  mutate(
    country_name = "Denmark",
    micro_code = row_number()
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = name
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Europe

respecEnv$geomEurope <- sf::st_read("countries/data/orig_geom/geomEurope.geojson")
namesEurope <- vroom::vroom("countries/data/namesEurope.csv")

respecEnv$geomEurope <- respecEnv$geomEurope %>%
  dplyr::inner_join(namesEurope, by = "UID") %>%
  dplyr::mutate(CountryName = case_when(
    CountryName == "Moldova" ~ "Republic of Moldova",
    CountryName == "Russian Fed." ~ "Russian Federation",
    CountryName == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    TRUE ~ CountryName
  )) %>%
  dplyr::left_join(
    m49,
    by = c("CountryName" = "CountryorArea")
  ) %>%
  dplyr::mutate(
    CountryName = dplyr::case_when(
      CountryName == "Republic of Moldova" ~ "Moldova",
      CountryName == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      CountryName == "Russian Federation" ~ "Russia",
      TRUE ~ CountryName
      )
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name = CountryName,
    micro_code = UID,
    micro_name = Region
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))
## Make Europe valid
respecEnv$geomEurope <- respecEnv$geomEurope %>%
  sf::st_transform("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>% # ESRI 102014: Europe Lambert Conformal Conic
  sf::st_make_valid() %>%
  sf::st_transform(4326)

rm(namesEurope)

# France

respecEnv$geomFrance <- sf::st_read("countries/data/orig_geom/geomFrance.geojson")

respecEnv$geomFrance <- respecEnv$geomFrance %>%
  dplyr::mutate(
    country_name = "France"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = code,
    micro_name = nom
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Germany

respecEnv$geomGermany <- sf::st_read("countries/data/orig_geom/geomGermany.geojson")

respecEnv$geomGermany <- respecEnv$geomGermany %>%
  dplyr::mutate(
    country_name = "Germany",
    micro_code = row_number()
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = county
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# India

respecEnv$geomIndia <- sf::st_read("countries/data/orig_geom/geomIndia.geojson")

respecEnv$geomIndia <- respecEnv$geomIndia %>%
  dplyr::group_by(NAME_1) %>% # Merge the two Dadra... so they only take up one entry
  dplyr::summarise() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  dplyr::mutate(
    country_name = "India",
    micro_code = row_number()
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = NAME_1
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Indonesia

respecEnv$geomIndonesia <- sf::st_read("countries/data/orig_geom/geomIndonesia.geojson")

respecEnv$geomIndonesia <- respecEnv$geomIndonesia %>%
  dplyr::mutate(
    country_name = "Indonesia",
    micro_code = row_number()
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = NAME_1
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Ireland

respecEnv$geomIreland <- sf::st_read("countries/data/orig_geom/geomIreland.geojson")

provIreland <- respecEnv$geomIreland %>%
  dplyr::group_by(PROVINCE) %>%
  dplyr::group_keys() %>%
  tibble::rowid_to_column("macro_code")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscIreland.csv")) {
  respecEnv$geomIreland %>%
    sf::st_drop_geometry() %>%
    write_csv("countries/data/miscIreland.csv")
  cat("\nmiscIreland.csv Created\n")
}else{
  cat("\nmiscIreland.csv Already Exists\n")
}

respecEnv$geomIreland <- respecEnv$geomIreland %>%
  dplyr::mutate(
    country_name = "Ireland"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::left_join(provIreland) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code,
    macro_name = PROVINCE,
    micro_code = CO_ID,
    micro_name = id
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))
## Make Ireland valid
respecEnv$geomIreland <- respecEnv$geomIreland %>%
  sf::st_transform(23029) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

rm(provIreland)

# Italy

respecEnv$geomItaly <- sf::st_read("countries/data/orig_geom/geomItaly.geojson")

respecEnv$geomItaly <- respecEnv$geomItaly %>%
  dplyr::mutate(
    country_name = "Italy"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = reg_istat_code,
    macro_name = reg_name,
    micro_code = prov_istat_code,
    micro_name = prov_name
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Japan

respecEnv$geomJapan <- sf::st_read("countries/data/orig_geom/geomJapan.geojson")

respecEnv$geomJapan <- respecEnv$geomJapan %>%
  dplyr::mutate(
    country_name = "Japan",
    micro_code = row_number()
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = NAME_1
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

## Make Japan valid
respecEnv$geomJapan <- respecEnv$geomJapan %>%
  sf::st_transform(3100) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# Malaysia

respecEnv$geomMalaysia <- sf::st_read("countries/data/orig_geom/geomMalaysia.geojson")

respecEnv$geomMalaysia <- respecEnv$geomMalaysia %>%
  dplyr::mutate(
    country_name = "Malaysia"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = id,
    micro_name = Name
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Mexico

respecEnv$geomMexico <- sf::st_read("countries/data/orig_geom/geomMexico.geojson")

respecEnv$geomMexico <- respecEnv$geomMexico %>%
  dplyr::mutate(
    country_name = "Mexico",
    macro_code = str_sub(CVEGEO, 1, 2)
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code,
    macro_name = estado,
    micro_code = CVEGEO,
    micro_name = NOMGEO
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

## Make Mexico valid
respecEnv$geomMexico <- respecEnv$geomMexico %>%
  sf::st_transform(6372) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# Mozambique

respecEnv$geomMozambique <- sf::st_read("countries/data/orig_geom/geomMozambique.geojson")

respecEnv$geomMozambique <- respecEnv$geomMozambique %>%
  dplyr::mutate(
    country_name = "Mozambique"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = ID_1,
    micro_name = NAME_1
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Netherlands

respecEnv$geomNetherlands <- sf::st_read("countries/data/orig_geom/geomNetherlands.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscNetherlands.csv")) {
  respecEnv$geomNetherlands %>%
    sf::st_drop_geometry() %>%
    write_csv("countries/data/miscNetherlands.csv")
  cat("\nmiscNetherlands.csv Created\n")
}else{
  cat("\nmiscNetherlands.csv Already Exists\n")
}

respecEnv$geomNetherlands <- respecEnv$geomNetherlands %>%
  dplyr::mutate(
    country_name = "Netherlands"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = Provinciecode,
    macro_name = Provincie,
    micro_code = Gemeentecode,
    micro_name = Gemeentenaam
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Nigeria

respecEnv$geomNigeria <- sf::st_read("countries/data/orig_geom/geomNigeria.geojson")

respecEnv$geomNigeria <- respecEnv$geomNigeria %>%
  dplyr::mutate(
    country_name = "Nigeria"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = admin1Pcod,
    micro_name = admin1Name
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Norway

respecEnv$geomNorway <- sf::st_read("countries/data/orig_geom/geomNorway.geojson")
## this geometry is missing Svalbard and Ukjent, but they also don't have population in the dataset
norw_kommune <- vroom::vroom("https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality.csv") %>%
  dplyr::select(kommune_no, kommune_name, fylke_no, fylke_name) %>%
  dplyr::distinct()

respecEnv$geomNorway <- respecEnv$geomNorway %>%
  dplyr::group_by(kommunenummer) %>%
  dplyr::summarise() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  dplyr::mutate(
    country_name = "Norway",
    kommune_no = str_pad(kommunenummer, pad = "0", side = "left", width = 4)
  ) %>%
  dplyr::left_join(
    norw_kommune
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = fylke_no,
    macro_name = fylke_name,
    micro_code = kommune_no,
    micro_name = kommune_name
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

rm(norw_kommune)

# NZ

respecEnv$geomNZ <- sf::st_read("countries/data/orig_geom/geomNZ.geojson")

respecEnv$geomNZ <- respecEnv$geomNZ %>%
  dplyr::mutate(
    country_name = "New Zealand"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = DHB2015_Co,
    micro_name = DHB2015_Na
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Peru

respecEnv$geomPeru <- sf::st_read("countries/data/orig_geom/geomPeru.geojson")

respecEnv$geomPeru <- respecEnv$geomPeru %>%
  dplyr::mutate(
    country_name = "Peru",
    NOMBDEP = str_replace_all(
      str_to_title(NOMBDEP),
      "\\sDe\\s",
      " de "
    )
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = FIRST_IDDP,
    micro_name = NOMBDEP
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Philippines

respecEnv$geomPhilippines <- sf::st_read("countries/data/orig_geom/geomPhilippines.geojson")
namesPhilippines <- vroom::vroom("countries/data/namesPhilippines.csv") %>%
  dplyr::select(starts_with("ADM2"), starts_with("ADM1")) %>%
  dplyr::distinct()

respecEnv$geomPhilippines <- respecEnv$geomPhilippines %>%
  dplyr::mutate(
    country_name = "Philippines"
  ) %>%
  dplyr::left_join(namesPhilippines) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = ADM1_PCODE,
    macro_name = ADM1_EN,
    micro_code = ADM2_PCODE,
    micro_name = ADM2_EN
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

rm(namesPhilippines)

# SaudiArabia

respecEnv$geomSaudiArabia <- sf::st_read("countries/data/orig_geom/geomSaudiArabia.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscSaudiArabia.csv")) {
  geomSaudiArabia %>%
    sf::st_drop_geometry() %>%
    write_csv("countries/data/miscSaudiArabia.csv")
  cat("\nmiscSaudiArabia.csv Created\n")
}else{
  cat("\nmiscSaudiArabia.csv Already Exists\n")
}

respecEnv$geomSaudiArabia <- respecEnv$geomSaudiArabia %>%
  dplyr::mutate(
    country_name = "Saudi Arabia"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = REG_CODE,
    micro_name = region_name_en
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character)) %>%
  sf::st_make_valid() # Make Saudi Arabia valid

# SouthAfrica

respecEnv$geomSouthAfrica <- sf::st_read("countries/data/orig_geom/geomSouthAfrica.geojson")

respecEnv$geomSouthAfrica <- respecEnv$geomSouthAfrica %>%
  dplyr::mutate(
    country_name = "South Africa"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = PROVINCE
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# Spain

respecEnv$geomSpain <- sf::st_read("countries/data/orig_geom/geomSpain.geojson")
namesSpain <- vroom::vroom("countries/data/namesSpain.csv") %>%
  dplyr::mutate(cod_ccaa = stringr::str_pad(Code, width = 2, side = "left", pad = "0"))

respecEnv$geomSpain <- respecEnv$geomSpain %>%
  dplyr::mutate(
    country_name = "Spain"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::left_join(namesSpain) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = cod_ccaa,
    macro_name = Region,
    micro_code = cod_prov,
    micro_name = name
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character)) %>%
  sf::st_make_valid() # Make Spain valid

rm(namesSpain)

# Sweden

respecEnv$geomSweden <- sf::st_read("countries/data/orig_geom/geomSweden.geojson")

respecEnv$geomSweden <- respecEnv$geomSweden %>%
  dplyr::mutate(
    country_name = "Sweden"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = cartodb_id,
    micro_name = name
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# SwitzerlandLiechtenstein

respecEnv$geomSwitzerlandLiechtenstein <- sf::st_read("countries/data/orig_geom/geomSwitzerlandLiechtenstein.geojson")

respecEnv$geomSwitzerlandLiechtenstein <- respecEnv$geomSwitzerlandLiechtenstein %>%
  dplyr::mutate(
    country_name = case_when(
      name == "Liechtenstein" ~ "Liechtenstein",
      TRUE ~ "Switzerland"
    )
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = id,
    micro_name = name
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

## Make Switzerland and Liechtenstein valid
respecEnv$geomSwitzerlandLiechtenstein <- respecEnv$geomSwitzerlandLiechtenstein %>%
  sf::st_transform(21781) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# Taiwan"

respecEnv$geomTaiwan <- sf::st_read("countries/data/orig_geom/geomTaiwan.geojson")

respecEnv$geomTaiwan <- respecEnv$geomTaiwan %>%
  dplyr::mutate(
    country_name = "Taiwan",
    m49code = 158, # Using ISO3166 code bc Taiwan is not in the M49 list
    iso3 = "TWN"
  ) %>%
  dplyr::select(
    m49code,
    iso3,
    country_name,
    micro_code = COUNTYSN,
    micro_name = COUNTYNAME
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character)) %>%
  dplyr::filter(!micro_code %in% c("10011001", "10006003"))

# Thailand"

respecEnv$geomThailand <- sf::st_read("countries/data/orig_geom/geomThailand.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscThailand.csv")) {
  respecEnv$geomThailand %>%
    sf::st_drop_geometry() %>%
    write_csv("countries/data/miscThailand.csv")
  cat("\nmiscThailand.csv Created\n")
}else{
  cat("\nmiscThailand.csv Already Exists\n")
}

respecEnv$geomThailand <- respecEnv$geomThailand %>%
  dplyr::mutate(
    country_name = "Thailand"
  ) %>%
  dplyr::left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_name = reg_nesdb,
    micro_code = pro_code,
    micro_name = pro_en
  ) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

# UnitedKingdom"

respecEnv$geomUnitedKingdom <- sf::st_read("countries/data/orig_geom/geomUnitedKingdom.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscUK.csv")) {
  respecEnv$geomUnitedKingdom %>%
    sf::st_drop_geometry() %>%
    write_csv("countries/data/miscUK.csv")
  cat("\nmiscUK.csv Created\n")
}else{
  cat("\nmiscUK.csv Already Exists\n")
}

respecEnv$geomUnitedKingdom <- respecEnv$geomUnitedKingdom %>%
  dplyr::mutate(
    country_name = "United Kingdom",
    m49code = m49$M49Code[which(m49$CountryorArea == "United Kingdom of Great Britain and Northern Ireland")],
    iso3 = m49$ISOalpha3Code[which(m49$CountryorArea == "United Kingdom of Great Britain and Northern Ireland")]
  ) %>%
  dplyr::select(
    m49code,
    iso3,
    country_name,
    micro_code = code,
    micro_name = name
  ) %>%
  dplyr::mutate(across(.cols = dplyr::ends_with("code"), .fns = as.character))

# UnitedStates"

respecEnv$geomUnitedStates <- sf::st_read("countries/data/orig_geom/geomUnitedStates.geojson")

usStatelines <- sf::st_read("countries/data/orig_geom/US_stateLines.geojson") %>%
  st_drop_geometry()

respecEnv$geomUnitedStates <- respecEnv$geomUnitedStates %>%
  dplyr::mutate(
    country_name = "United States",
    m49code = m49$M49Code[which(m49$CountryorArea == "United States of America")],
    iso3 = m49$ISOalpha3Code[which(m49$CountryorArea == "United States of America")]
  ) %>%
  dplyr::inner_join(usStatelines, by = c("stname" = "STUSPS")) %>%
  dplyr::select(
    m49code,
    iso3,
    country_name,
    macro_code = stname,
    macro_name = NAME.y,
    micro_code = GEOID.x,
    micro_name = NAME.x
  ) %>%
  dplyr::mutate(across(.cols = dplyr::ends_with("code"), .fns = as.character))

respecEnv$geomAK <- respecEnv$geomUnitedStates %>%
  dplyr::filter(macro_code == "AK") %>%
  sf::st_transform(3467) %>%
  dplyr::group_by(m49code, iso3, country_name, macro_code, macro_name, micro_code, micro_name) %>%
  dplyr::summarise() %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

respecEnv$geomCO <- respecEnv$geomUnitedStates %>%
  dplyr::filter(macro_code == "CO") %>%
  sf::st_transform(2773) %>%
  dplyr::group_by(m49code, iso3, country_name, macro_code, macro_name, micro_code, micro_name) %>%
  dplyr::summarise() %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

respecEnv$geomID <- respecEnv$geomUnitedStates %>%
  dplyr::filter(macro_code == "ID") %>%
  sf::st_transform(2788) %>%
  dplyr::group_by(m49code, iso3, country_name, macro_code, macro_name, micro_code, micro_name) %>%
  dplyr::summarise() %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

respecEnv$geomKS <- respecEnv$geomUnitedStates %>%
  dplyr::filter(macro_code == "KS") %>%
  sf::st_make_valid()

respecEnv$geomUnitedStates <- respecEnv$geomUnitedStates %>%
  dplyr::filter(!macro_code %in% c("AK", "CO", "ID", "KS")) %>%
  dplyr::bind_rows(respecEnv$geomAK, respecEnv$geomCO, respecEnv$geomID, respecEnv$geomKS)

rm(list = c("geomAK", "geomCO", "geomID", "geomKS"), envir = respecEnv)
rm(usStatelines)
# Venezuela"

respecEnv$geomVenezuela <- sf::st_read("countries/data/orig_geom/geomVenezuela.geojson")

respecEnv$geomVenezuela <- respecEnv$geomVenezuela %>%
  dplyr::mutate(
    country_name = "Venezuela",
    m49code = m49$M49Code[which(m49$CountryorArea == "Venezuela (Bolivarian Republic of)")],
    iso3 = m49$ISOalpha3Code[which(m49$CountryorArea == "Venezuela (Bolivarian Republic of)")]
  ) %>%
  dplyr::select(
    m49code,
    iso3,
    country_name,
    micro_code = ID_1,
    micro_name = NAME_1
  ) %>%
  dplyr::mutate(across(.cols = dplyr::ends_with("code"), .fns = as.character))

## Make Venezuela valid
respecEnv$geomVenezuela <- respecEnv$geomVenezuela %>%
  sf::st_transform(2201) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# Small Countries
respecEnv$geomSmallCountries <- sf::st_read("countries/data/orig_geom/WB_countries_Admin0_lowres.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/popSmallCountries.csv")) {
  respecEnv$geomSmallCountries %>%
    sf::st_drop_geometry() %>%
    write_csv("countries/data/popSmallCountries.csv")
  cat("\npopSmallCountries.csv Created\n")
}else{
  cat("\npopSmallCountries.csv Already Exists\n")
}

respecEnv$geomSmallCountries <- respecEnv$geomSmallCountries %>%
  dplyr::left_join(m49, by = c("ISO_A3" = "ISOalpha3Code")) %>%
  dplyr::select(
    m49code = M49Code,
    iso3 = ISO_A3,
    country_name = NAME_EN
  )

respecEnv$geomFiji <- respecEnv$geomSmallCountries %>%
  dplyr::filter(iso3 == "FJI") %>% # Fiji
  sf::st_cast("POLYGON") %>%
  sf::st_wrap_dateline() %>%
  dplyr::group_by(m49code, iso3, country_name) %>%
  dplyr::summarise() %>%
  dplyr::ungroup()

respecEnv$geomSmallCountries <- respecEnv$geomSmallCountries[which(sf::st_is_valid(respecEnv$geomSmallCountries, reason = T) == "Valid Geometry"), ] %>% # Russia, USA, and Japan are accounted for in other data sets, so we can afford to loose them
  dplyr::bind_rows(respecEnv$geomFiji) %>%
  dplyr::mutate(across(.cols = ends_with("code"), .fns = as.character))

rm(geomFiji, envir = respecEnv)

# Tidy up and finish

## the respecEnv has all the country geoms *and only the country geoms*
geom_list <- ls(respecEnv)

geomWorld <- purrr::map_df(
  geom_list, # For each geometry file...
  ~ get(.x, envir = respecEnv) %>%
    dplyr::mutate(
      filename = .x # Add a column with the filename...
    ) %>%
    { if(st_geometry_type(., FALSE) %in% c("GEOMETRY", "GEOMETRYCOLLECTION")){
      sf::st_collection_extract(.) # If geometry type "geometry" or "geometrycollection," cast back to polygon...
      }else{.}
      } %>%
    sf::st_cast("MULTIPOLYGON") # Cast everything to Multipolygon so they are all the same type
) %>%
  replace_na(list("macro_code" = "00")) %>% # Replace NAs in macro_code with "00" 
  dplyr::mutate( # Create GEOIDs
    geoid = paste(paste0(iso3, m49code), macro_code, micro_code, sep = "_")
  ) %>%
  dplyr::select(geoid, m49code, iso3, country_name, macro_code, macro_name, everything()) %>%
  sf::st_as_sf() %>%
  tibble::remove_rownames()

## Write presimplified geometry
sf::st_write(geomWorld, "geomWorld_presimp.geojson", delete_dsn = T)
rm(respecEnv) ## delete environment with individual geometries

## Zip presimplified geometry
nowtime <- round(difftime(now(tzone = "UTC"), ymd_hms("1970-01-01 00:00:00"), tz = "UTC", units = "mins"))
utils::zip(zipfile = paste0("updateGeometry/WorldPreSimplified/WorldPreSimp", nowtime, ".zip"), "geomWorld_presimp.geojson")
file.remove("geomWorld_presimp.geojson")
rm(m49)
rm(geom_list)
