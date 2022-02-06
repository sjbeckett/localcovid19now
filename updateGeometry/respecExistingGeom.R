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

respecEnv$geomAfghanistan <- st_read("countries/data/orig_geom/geomAfghanistan.geojson")

respecEnv$geomAfghanistan <- respecEnv$geomAfghanistan %>%
  mutate(
    country_name = "Afghanistan",
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = id,
    micro_name = name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Algeria

respecEnv$geomAlgeria <- st_read("countries/data/orig_geom/geomAlgeria.geojson")

respecEnv$geomAlgeria <- respecEnv$geomAlgeria %>%
  mutate(
    country_name = "Algeria",
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = ID,
    micro_name = NAME
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Argentina

respecEnv$geomArgentina <- st_read("countries/data/orig_geom/geomArgentina.geojson")
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
  mutate(
    country_name = "Argentina",
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = ID_1,
    micro_name = NAME_1
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Australia

respecEnv$geomAustralia <- st_read("countries/data/orig_geom/geomAustralia.geojson")

respecEnv$geomAustralia <- respecEnv$geomAustralia %>%
  mutate(
    country_name = "Australia",
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = STATE_CODE,
    micro_name = STATE_NAME
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Austria

respecEnv$geomAustria <- st_read("countries/data/orig_geom/geomAustria.geojson")

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

respecEnv$geomBelgium <- st_read("countries/data/orig_geom/geomBelgium.geojson")

respecEnv$geomBelgium <- respecEnv$geomBelgium %>%
  mutate(
    country_name = "Belgium",
    micro_code = row_number() # I'm assigning IDs based on row number, but we could go back and do country subdivision ISOs
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = NameDUT
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Brazil

respecEnv$geomBrazil <- st_read("countries/data/orig_geom/geomBrazil.geojson")

respecEnv$geomBrazil <- respecEnv$geomBrazil %>%
  mutate(
    country_name = "Brazil"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = regiao_id,
    micro_code = sigla,
    micro_name = nome
  ) %>%
  st_make_valid() %>% # Brazil has some invalidity
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Canada

respecEnv$geomCanada <- st_read("countries/data/orig_geom/geomCanada.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscCanada.csv")) {
  respecEnv$geomCanada %>%
    st_drop_geometry() %>%
    write_csv("countries/data/miscCanada.csv")
  cat("\nmiscCanada.csv Created\n")
}else{
  cat("\nmiscCanada.csv Already Exists\n")
}

respecEnv$geomCanada <- respecEnv$geomCanada %>%
  mutate(
    country_name = "Canada"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = Province,
    micro_code = HR_UID,
    micro_name = ENGNAME
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))
# Make canada valid
respecEnv$geomCanada <- respecEnv$geomCanada %>%
  st_transform(3347) %>%
  st_make_valid() %>%
  st_transform(4326)

# Chile

respecEnv$geomChile <- st_read("countries/data/orig_geom/geomChile.geojson")

if (!file.exists("countries/data/miscChile.csv")) {
  respecEnv$geomChile %>%
    st_drop_geometry() %>%
    write_csv("countries/data/miscChile.csv")
  cat("\nmiscChile.csv Created\n")
}else{
  cat("\nmiscChile.csv Already Exists\n")
}

respecEnv$geomChile <- respecEnv$geomChile %>%
  mutate(
    country_name = "Chile"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = codregion,
    micro_name = matchName
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))
# Make Chile valid
respecEnv$geomChile <- respecEnv$geomChile %>%
  st_transform("+proj=utm +zone=23 +south +ellps=aust_SA +units=m +no_defs") %>%
  st_make_valid() %>%
  st_transform(4326)

# China

respecEnv$geomChina <- st_read("countries/data/orig_geom/geomChina.geojson")

## Only create csv if it doesn't exist.
if (!file.exists("countries/data/miscChina.csv")) {
  respecEnv$geomChina %>%
    st_drop_geometry() %>%
    write_csv("countries/data/miscChina.csv")
  cat("\nmiscChina.csv Created\n")
}else{
  cat("\nmiscChina.csv Already Exists\n")
}

respecEnv$geomChina <- respecEnv$geomChina %>%
  mutate(
    country_name = "China"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = ISO,
    micro_name = NAME_1
  ) %>%
  filter(micro_code != "CN-71") %>% # Remove Taiwan because we have higher resolution in geomTaiwan
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Colombia

respecEnv$geomColombia <- st_read("countries/data/orig_geom/geomColombia.geojson")

respecEnv$geomColombia <- respecEnv$geomColombia %>%
  mutate(
    country_name = "Colombia",
    NOMBRE_DPT = str_to_title(NOMBRE_DPT)
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = DPTO,
    micro_name = NOMBRE_DPT
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Cuba

respecEnv$geomCuba <- st_read("countries/data/orig_geom/geomCuba.geojson")

respecEnv$geomCuba <- respecEnv$geomCuba %>%
  mutate(
    country_name = "Cuba",
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = province_id,
    micro_name = province
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character)) %>%
  filter(micro_code != "unk")

## Make Cuba valid
respecEnv$geomCuba <- respecEnv$geomCuba %>%
  st_transform(32617) %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  st_make_valid()

# Czechia

respecEnv$geomCzechia <- st_read("countries/data/orig_geom/geomCzechia.geojson")

namesCzechia <- vroom("countries/data/czech_pop.csv") %>%
  select(-Population)
## Load codes for Local Admin Units (LAU)
respecEnv$geomCzechia <- respecEnv$geomCzechia %>%
  full_join(namesCzechia, by = c("name" = "District")) %>%
  mutate(
    country_name = "Czechia",
    macro_code = str_sub(Code, 1, 5) # NUTS 3 level code
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code,
    micro_code = Code,
    micro_name = name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

rm(namesCzechia)

# Denmark

respecEnv$geomDenmark <- st_read("countries/data/orig_geom/geomDenmark.geojson")

respecEnv$geomDenmark <- respecEnv$geomDenmark %>%
  st_zm() %>%
  group_by(name) %>%
  summarise() %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(
    country_name = "Denmark",
    micro_code = row_number()
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Europe

respecEnv$geomEurope <- st_read("countries/data/orig_geom/geomEurope.geojson")
namesEurope <- vroom::vroom("countries/data/namesEurope.csv")

respecEnv$geomEurope <- respecEnv$geomEurope %>%
  inner_join(namesEurope, by = "UID") %>%
  mutate(CountryName = case_when(
    CountryName == "Moldova" ~ "Republic of Moldova",
    CountryName == "Russian Fed." ~ "Russian Federation",
    CountryName == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    TRUE ~ CountryName
  )) %>%
  left_join(
    m49,
    by = c("CountryName" = "CountryorArea")
  ) %>%
  mutate(
    CountryName = case_when(
      CountryName == "Republic of Moldova" ~ "Moldova",
      CountryName == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      CountryName == "Russian Federation" ~ "Russia",
      TRUE ~ CountryName
      )
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name = CountryName,
    micro_code = UID,
    micro_name = Region
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))
## Make Europe valid
respecEnv$geomEurope <- respecEnv$geomEurope %>%
  st_transform("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>% # ESRI 102014: Europe Lambert Conformal Conic
  st_make_valid() %>%
  st_transform(4326)

rm(namesEurope)

# France

respecEnv$geomFrance <- st_read("countries/data/orig_geom/geomFrance.geojson")

respecEnv$geomFrance <- respecEnv$geomFrance %>%
  mutate(
    country_name = "France"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = code,
    micro_name = nom
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Germany

respecEnv$geomGermany <- st_read("countries/data/orig_geom/geomGermany.geojson")

respecEnv$geomGermany <- respecEnv$geomGermany %>%
  mutate(
    country_name = "Germany",
    micro_code = row_number()
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = county
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# India

respecEnv$geomIndia <- st_read("countries/data/orig_geom/geomIndia.geojson")

respecEnv$geomIndia <- respecEnv$geomIndia %>%
  group_by(NAME_1) %>% # Merge the two Dadra... so they only take up one entry
  summarise() %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(
    country_name = "India",
    micro_code = row_number()
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = NAME_1
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Indonesia

respecEnv$geomIndonesia <- st_read("countries/data/orig_geom/geomIndonesia.geojson")

respecEnv$geomIndonesia <- respecEnv$geomIndonesia %>%
  mutate(
    country_name = "Indonesia",
    micro_code = row_number()
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = NAME_1
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Ireland

respecEnv$geomIreland <- st_read("countries/data/orig_geom/geomIreland.geojson")

provIreland <- respecEnv$geomIreland %>%
  group_by(PROVINCE) %>%
  group_keys() %>%
  tibble::rowid_to_column("macro_code")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscIreland.csv")) {
  respecEnv$geomIreland %>%
    st_drop_geometry() %>%
    write_csv("countries/data/miscIreland.csv")
  cat("\nmiscIreland.csv Created\n")
}else{
  cat("\nmiscIreland.csv Already Exists\n")
}

respecEnv$geomIreland <- respecEnv$geomIreland %>%
  mutate(
    country_name = "Ireland"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  left_join(provIreland) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code,
    macro_name = PROVINCE,
    micro_code = CO_ID,
    micro_name = id
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))
## Make Ireland valid
respecEnv$geomIreland <- respecEnv$geomIreland %>%
  st_transform(23029) %>%
  st_make_valid() %>%
  st_transform(4326)

rm(provIreland)

# Italy

respecEnv$geomItaly <- st_read("countries/data/orig_geom/geomItaly.geojson")

respecEnv$geomItaly <- respecEnv$geomItaly %>%
  mutate(
    country_name = "Italy"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = reg_istat_code,
    macro_name = reg_name,
    micro_code = prov_istat_code,
    micro_name = prov_name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Japan

respecEnv$geomJapan <- st_read("countries/data/orig_geom/geomJapan.geojson")

respecEnv$geomJapan <- respecEnv$geomJapan %>%
  mutate(
    country_name = "Japan",
    micro_code = row_number()
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code,
    micro_name = NAME_1
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

## Make Japan valid
respecEnv$geomJapan <- respecEnv$geomJapan %>%
  st_transform(3100) %>%
  st_make_valid() %>%
  st_transform(4326)

# Malaysia

respecEnv$geomMalaysia <- st_read("countries/data/orig_geom/geomMalaysia.geojson")

respecEnv$geomMalaysia <- respecEnv$geomMalaysia %>%
  mutate(
    country_name = "Malaysia"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = id,
    micro_name = Name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Mexico

respecEnv$geomMexico <- st_read("countries/data/orig_geom/geomMexico.geojson")

respecEnv$geomMexico <- respecEnv$geomMexico %>%
  mutate(
    country_name = "Mexico",
    macro_code = str_sub(CVEGEO, 1, 2)
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code,
    macro_name = estado,
    micro_code = CVEGEO,
    micro_name = NOMGEO
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

## Make Mexico valid
respecEnv$geomMexico <- respecEnv$geomMexico %>%
  st_transform(6372) %>%
  st_make_valid() %>%
  st_transform(4326)

# Mozambique

respecEnv$geomMozambique <- st_read("countries/data/orig_geom/geomMozambique.geojson")

respecEnv$geomMozambique <- respecEnv$geomMozambique %>%
  mutate(
    country_name = "Mozambique"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = ID_1,
    micro_name = NAME_1
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Netherlands

respecEnv$geomNetherlands <- st_read("countries/data/orig_geom/geomNetherlands.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscNetherlands.csv")) {
  respecEnv$geomNetherlands %>%
    st_drop_geometry() %>%
    write_csv("countries/data/miscNetherlands.csv")
  cat("\nmiscNetherlands.csv Created\n")
}else{
  cat("\nmiscNetherlands.csv Already Exists\n")
}

respecEnv$geomNetherlands <- respecEnv$geomNetherlands %>%
  mutate(
    country_name = "Netherlands"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = Provinciecode,
    macro_name = Provincie,
    micro_code = Gemeentecode,
    micro_name = Gemeentenaam
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Nigeria

respecEnv$geomNigeria <- st_read("countries/data/orig_geom/geomNigeria.geojson")

respecEnv$geomNigeria <- respecEnv$geomNigeria %>%
  mutate(
    country_name = "Nigeria"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = admin1Pcod,
    micro_name = admin1Name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Norway

respecEnv$geomNorway <- st_read("countries/data/orig_geom/geomNorway.geojson")
## this geometry is missing Svalbard and Ukjent, but they also don't have population in the dataset
norw_kommune <- vroom("https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality.csv") %>%
  select(kommune_no, kommune_name, fylke_no, fylke_name) %>%
  distinct()

respecEnv$geomNorway <- respecEnv$geomNorway %>%
  group_by(kommunenummer) %>%
  summarise() %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(
    country_name = "Norway",
    kommune_no = str_pad(kommunenummer, pad = "0", side = "left", width = 4)
  ) %>%
  left_join(
    norw_kommune
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = fylke_no,
    macro_name = fylke_name,
    micro_code = kommune_no,
    micro_name = kommune_name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

rm(norw_kommune)

# NZ

respecEnv$geomNZ <- st_read("countries/data/orig_geom/geomNZ.geojson")

respecEnv$geomNZ <- respecEnv$geomNZ %>%
  mutate(
    country_name = "New Zealand"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = DHB2015_Co,
    micro_name = DHB2015_Na
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Peru

respecEnv$geomPeru <- st_read("countries/data/orig_geom/geomPeru.geojson")

respecEnv$geomPeru <- respecEnv$geomPeru %>%
  mutate(
    country_name = "Peru",
    NOMBDEP = str_replace_all(
      str_to_title(NOMBDEP),
      "\\sDe\\s",
      " de "
    )
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = FIRST_IDDP,
    micro_name = NOMBDEP
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Philippines

respecEnv$geomPhilippines <- st_read("countries/data/orig_geom/geomPhilippines.geojson")
namesPhilippines <- vroom("countries/data/namesPhilippines.csv") %>%
  select(starts_with("ADM2"), starts_with("ADM1")) %>%
  distinct()

respecEnv$geomPhilippines <- respecEnv$geomPhilippines %>%
  mutate(
    country_name = "Philippines"
  ) %>%
  left_join(namesPhilippines) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = ADM1_PCODE,
    macro_name = ADM1_EN,
    micro_code = ADM2_PCODE,
    micro_name = ADM2_EN
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

rm(namesPhilippines)

# SaudiArabia

respecEnv$geomSaudiArabia <- st_read("countries/data/orig_geom/geomSaudiArabia.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscSaudiArabia.csv")) {
  geomSaudiArabia %>%
    st_drop_geometry() %>%
    write_csv("countries/data/miscSaudiArabia.csv")
  cat("\nmiscSaudiArabia.csv Created\n")
}else{
  cat("\nmiscSaudiArabia.csv Already Exists\n")
}

respecEnv$geomSaudiArabia <- respecEnv$geomSaudiArabia %>%
  mutate(
    country_name = "Saudi Arabia"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = REG_CODE,
    micro_name = region_name_en
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character)) %>%
  st_make_valid() # Make Saudi Arabia valid

# SouthAfrica

respecEnv$geomSouthAfrica <- st_read("countries/data/orig_geom/geomSouthAfrica.geojson")

respecEnv$geomSouthAfrica <- respecEnv$geomSouthAfrica %>%
  mutate(
    country_name = "South Africa"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = PROVINCE
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# Spain

respecEnv$geomSpain <- st_read("countries/data/orig_geom/geomSpain.geojson")
namesSpain <- vroom::vroom("countries/data/namesSpain.csv") %>%
  mutate(cod_ccaa = str_pad(Code, width = 2, side = "left", pad = "0"))

respecEnv$geomSpain <- respecEnv$geomSpain %>%
  mutate(
    country_name = "Spain"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  left_join(namesSpain) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_code = cod_ccaa,
    macro_name = Region,
    micro_code = cod_prov,
    micro_name = name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character)) %>%
  st_make_valid() # Make Spain valid

rm(namesSpain)

# Sweden

respecEnv$geomSweden <- st_read("countries/data/orig_geom/geomSweden.geojson")

respecEnv$geomSweden <- respecEnv$geomSweden %>%
  mutate(
    country_name = "Sweden"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = cartodb_id,
    micro_name = name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# SwitzerlandLiechtenstein

respecEnv$geomSwitzerlandLiechtenstein <- st_read("countries/data/orig_geom/geomSwitzerlandLiechtenstein.geojson")

respecEnv$geomSwitzerlandLiechtenstein <- respecEnv$geomSwitzerlandLiechtenstein %>%
  mutate(
    country_name = case_when(
      name == "Liechtenstein" ~ "Liechtenstein",
      TRUE ~ "Switzerland"
    )
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    micro_code = id,
    micro_name = name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

## Make Switzerland and Liechtenstein valid
respecEnv$geomSwitzerlandLiechtenstein <- respecEnv$geomSwitzerlandLiechtenstein %>%
  st_transform(21781) %>%
  st_make_valid() %>%
  st_transform(4326)

# Taiwan"

respecEnv$geomTaiwan <- st_read("countries/data/orig_geom/geomTaiwan.geojson")

respecEnv$geomTaiwan <- respecEnv$geomTaiwan %>%
  mutate(
    country_name = "Taiwan",
    m49code = 158, # Using ISO3166 code bc Taiwan is not in the M49 list
    iso3 = "TWN"
  ) %>%
  select(
    m49code,
    iso3,
    country_name,
    micro_code = COUNTYSN,
    micro_name = COUNTYNAME
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character)) %>%
  filter(!micro_code %in% c("10011001", "10006003"))

# Thailand"

respecEnv$geomThailand <- st_read("countries/data/orig_geom/geomThailand.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscThailand.csv")) {
  respecEnv$geomThailand %>%
    st_drop_geometry() %>%
    write_csv("countries/data/miscThailand.csv")
  cat("\nmiscThailand.csv Created\n")
}else{
  cat("\nmiscThailand.csv Already Exists\n")
}

respecEnv$geomThailand <- respecEnv$geomThailand %>%
  mutate(
    country_name = "Thailand"
  ) %>%
  left_join(
    m49,
    by = c("country_name" = "CountryorArea")
  ) %>%
  select(
    m49code = M49Code,
    iso3 = ISOalpha3Code,
    country_name,
    macro_name = reg_nesdb,
    micro_code = pro_code,
    micro_name = pro_en
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# UnitedKingdom"

respecEnv$geomUnitedKingdom <- st_read("countries/data/orig_geom/geomUnitedKingdom.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/miscUK.csv")) {
  respecEnv$geomUnitedKingdom %>%
    st_drop_geometry() %>%
    write_csv("countries/data/miscUK.csv")
  cat("\nmiscUK.csv Created\n")
}else{
  cat("\nmiscUK.csv Already Exists\n")
}

respecEnv$geomUnitedKingdom <- respecEnv$geomUnitedKingdom %>%
  mutate(
    country_name = "United Kingdom",
    m49code = m49$M49Code[which(m49$CountryorArea == "United Kingdom of Great Britain and Northern Ireland")],
    iso3 = m49$ISOalpha3Code[which(m49$CountryorArea == "United Kingdom of Great Britain and Northern Ireland")]
  ) %>%
  select(
    m49code,
    iso3,
    country_name,
    micro_code = code,
    micro_name = name
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

# UnitedStates"

respecEnv$geomUnitedStates <- st_read("countries/data/orig_geom/geomUnitedStates.geojson")

usStatelines <- st_read("countries/data/orig_geom/US_stateLines.geojson") %>%
  st_drop_geometry()

respecEnv$geomUnitedStates <- respecEnv$geomUnitedStates %>%
  mutate(
    country_name = "United States",
    m49code = m49$M49Code[which(m49$CountryorArea == "United States of America")],
    iso3 = m49$ISOalpha3Code[which(m49$CountryorArea == "United States of America")]
  ) %>%
  inner_join(usStatelines, by = c("stname" = "STUSPS")) %>%
  select(
    m49code,
    iso3,
    country_name,
    macro_code = stname,
    macro_name = NAME.y,
    micro_code = GEOID.x,
    micro_name = NAME.x
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

respecEnv$geomAK <- respecEnv$geomUnitedStates %>%
  filter(macro_code == "AK") %>%
  st_transform(3467) %>%
  group_by(m49code, iso3, country_name, macro_code, macro_name, micro_code, micro_name) %>%
  summarise() %>%
  st_make_valid() %>%
  st_transform(4326)

respecEnv$geomCO <- respecEnv$geomUnitedStates %>%
  filter(macro_code == "CO") %>%
  st_transform(2773) %>%
  group_by(m49code, iso3, country_name, macro_code, macro_name, micro_code, micro_name) %>%
  summarise() %>%
  st_make_valid() %>%
  st_transform(4326)

respecEnv$geomID <- respecEnv$geomUnitedStates %>%
  filter(macro_code == "ID") %>%
  st_transform(2788) %>%
  group_by(m49code, iso3, country_name, macro_code, macro_name, micro_code, micro_name) %>%
  summarise() %>%
  st_make_valid() %>%
  st_transform(4326)

respecEnv$geomKS <- respecEnv$geomUnitedStates %>%
  filter(macro_code == "KS") %>%
  st_make_valid()

respecEnv$geomUnitedStates <- respecEnv$geomUnitedStates %>%
  filter(!macro_code %in% c("AK", "CO", "ID", "KS")) %>%
  bind_rows(respecEnv$geomAK, respecEnv$geomCO, respecEnv$geomID, respecEnv$geomKS)

rm(list = c("geomAK", "geomCO", "geomID", "geomKS"), envir = respecEnv)
rm(usStatelines)
# Venezuela"

respecEnv$geomVenezuela <- st_read("countries/data/orig_geom/geomVenezuela.geojson")

respecEnv$geomVenezuela <- respecEnv$geomVenezuela %>%
  mutate(
    country_name = "Venezuela",
    m49code = m49$M49Code[which(m49$CountryorArea == "Venezuela (Bolivarian Republic of)")],
    iso3 = m49$ISOalpha3Code[which(m49$CountryorArea == "Venezuela (Bolivarian Republic of)")]
  ) %>%
  select(
    m49code,
    iso3,
    country_name,
    micro_code = ID_1,
    micro_name = NAME_1
  ) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

## Make Venezuela valid
respecEnv$geomVenezuela <- respecEnv$geomVenezuela %>%
  st_transform(2201) %>%
  st_make_valid() %>%
  st_transform(4326)

# Small Countries
respecEnv$geomSmallCountries <- st_read("countries/data/orig_geom/WB_countries_Admin0_lowres.geojson")

## Only create csv if it doesn't exist. 
if (!file.exists("countries/data/popSmallCountries.csv")) {
  respecEnv$geomSmallCountries %>%
    st_drop_geometry() %>%
    write_csv("countries/data/popSmallCountries.csv")
  cat("\npopSmallCountries.csv Created\n")
}else{
  cat("\npopSmallCountries.csv Already Exists\n")
}

respecEnv$geomSmallCountries <- respecEnv$geomSmallCountries %>%
  left_join(m49, by = c("ISO_A3" = "ISOalpha3Code")) %>%
  select(
    m49code = M49Code,
    iso3 = ISO_A3,
    country_name = NAME_EN
  )

respecEnv$geomFiji <- respecEnv$geomSmallCountries %>%
  filter(iso3 == "FJI") %>% # Fiji
  st_cast("POLYGON") %>%
  st_wrap_dateline() %>%
  group_by(m49code, iso3, country_name) %>%
  summarise() %>%
  ungroup()

respecEnv$geomSmallCountries <- respecEnv$geomSmallCountries[which(st_is_valid(respecEnv$geomSmallCountries, reason = T) == "Valid Geometry"), ] %>% # Russia, USA, and Japan are accounted for in other data sets, so we can afford to loose them
  bind_rows(respecEnv$geomFiji) %>%
  mutate(across(.cols = ends_with("code"), .fns = as.character))

rm(geomFiji, envir = respecEnv)

# Tidy up and finish

## the respecEnv has all the country geoms *and only the country geoms*
geom_list <- ls(respecEnv)

geomWorld <- purrr::map_df(
  geom_list, # For each geometry file...
  ~ get(.x, envir = respecEnv) %>%
    mutate(
      filename = .x # Add a column with the filename...
    ) %>%
    { if(st_geometry_type(., FALSE) %in% c("GEOMETRY", "GEOMETRYCOLLECTION")){
      st_collection_extract(.) # If geometry type "geometry" or "geometrycollection," cast back to polygon...
      }else{.}
      } %>%
    st_cast("MULTIPOLYGON") # Cast everything to Multipolygon so they are all the same type
) %>%
  replace_na(list("macro_code" = "00")) %>% # Replace NAs in macro_code with "00" 
  mutate( # Create GEOIDs
    geoid = paste(paste0(iso3, m49code), macro_code, micro_code, sep = "_")
  ) %>%
  select(geoid, m49code, iso3, country_name, macro_code, macro_name, everything()) %>%
  st_as_sf() %>%
  tibble::remove_rownames()

## Write presimplified geometry
st_write(geomWorld, "geomWorld_presimp.geojson", delete_dsn = T)
rm(respecEnv) ## delete environment with individual geometries

## Zip presimplified geometry
nowtime <- round(difftime(now(tzone = "UTC"), ymd_hms("1970-01-01 00:00:00"), tz = "UTC", units = "mins"))
zip(zipfile = paste0("updateGeometry/WorldPreSimplified/WorldPreSimp", nowtime, ".zip"), "geomWorld_presimp.geojson")
file.remove("geomWorld_presimp.geojson")
rm(m49)
rm(geom_list)
