source("librariesMinimal.R")
library(readr)

# Fields will be as follows:
## id
## m49_code
## county_name
## region_code
## region_name
## district_code
## district_name
## geometry

## Load in the m49 codes for countries
m49 <- readxl::read_xlsx("UNSD_m49.xlsx")
m49 <- m49%>%
  rename_with(
    .fn = \(x) str_replace_all(x, "[\\s/-]","")
  )
geom_list <- c()
# Afghanistan

geomAfghanistan <- st_read("countries/data/orig_geom/geomAfghanistan.geojson")

geomAfghanistan <- geomAfghanistan%>%
  mutate(
    country_name = "Afghanistan",
    filename = "geomAfghanistan"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = id,
    district_name = name,
    filename
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomAfghanistan%>%
  st_write("countries/data/temp_geom/geomAfghanistan.geojson")
# #rm(geomAfghanistan)

# Algeria
geomAlgeria <- st_read("countries/data/orig_geom/geomAlgeria.geojson")

geomAlgeria <- geomAlgeria%>%
  mutate(
    country_name = "Algeria",
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = ID,
    district_name = NAME
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))


geomAlgeria%>%
  st_write("countries/data/temp_geom/geomAlgeria.geojson")
# #rm(geomAlgeria)

# Argentina
geomArgentina <- st_read("countries/data/orig_geom/geomArgentina.geojson")

geomArgentina <- geomArgentina%>%
  mutate(
    country_name = "Argentina",
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = ID_1,
    district_name = NAME_1
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomArgentina%>%
  st_write("countries/data/temp_geom/geomArgentina.geojson")
# #rm(geomArgentina)

# Australia 
geomAustralia <- st_read("countries/data/orig_geom/geomAustralia.geojson")

geomAustralia <- geomAustralia%>%
  mutate(
    country_name = "Australia",
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = STATE_CODE,
    district_name = STATE_NAME
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))


geomAustralia%>%
  st_write("countries/data/temp_geom/geomAustralia.geojson")
# #rm(geomAustralia)

# Austria
geomAustria <- st_read("countries/data/orig_geom/geomAustria.geojson")

geomAustria <- geomAustria%>%
  mutate(
    country_name = "Austria",
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = iso,
    district_name = name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomAustria%>%
  st_write("countries/data/temp_geom/geomAustria.geojson")
#rm(geomAustria)

# Belgium
geomBelgium <- st_read("countries/data/orig_geom/geomBelgium.geojson")

geomBelgium <- geomBelgium%>%
  mutate(
    country_name = "Belgium",
    district_code = row_number() # I'm assigning IDs based on row number, but we could go back and do country subdivision ISOs
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code,
    district_name = NameDUT
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomBelgium%>%
  st_write("countries/data/temp_geom/geomBelgium.geojson")
# #rm(geomBelgium)


# Brazil"

geomBrazil <- st_read("countries/data/orig_geom/geomBrazil.geojson")

geomBrazil <- geomBrazil%>%
  mutate(
    country_name = "Brazil"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    region_code = regiao_id,
    district_code = codigo_ibg,
    district_name = nome
  )%>%
  st_make_valid()%>% # Brazil has some invalidity
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomBrazil%>%
  st_write("countries/data/temp_geom/geomBrazil.geojson")
# #rm(geomBrazil)
# 

# Canada"

geomCanada <- st_read("countries/data/orig_geom/geomCanada.geojson")

geomCanada%>%
  st_drop_geometry()%>%
  write_csv("countries/data/miscCanada.csv")

geomCanada <- geomCanada%>%
  mutate(
    country_name = "Canada"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = HR_UID,
    district_name = ENGNAME
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))
# Make canada valid
geomCanada <- geomCanada%>%
  st_transform(3347)%>% 
  st_make_valid()%>%
  st_transform(4326)


geomCanada%>%
  st_write("countries/data/temp_geom/geomCanada.geojson")
# #rm(geomCanada)

# Chile"

geomChile <- st_read("countries/data/orig_geom/geomChile.geojson")

geomChile <- geomChile%>%
  mutate(
    country_name = "Chile"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = codregion,
    district_name = matchName
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))
# Make Chile valid
geomChile <- geomChile%>%
  st_transform("+proj=utm +zone=23 +south +ellps=aust_SA +units=m +no_defs")%>%
  st_make_valid()%>%
  st_transform(4326)

geomChile%>%
  st_write("countries/data/temp_geom/geomChile.geojson")
# #rm(geomChile)

# China"

geomChina <- st_read("countries/data/orig_geom/geomChina.geojson")

geomChina <- geomChina%>%
  mutate(
    country_name = "China"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = ISO,
    district_name = NAME_1
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomChina%>%
  st_write("countries/data/temp_geom/geomChina.geojson")
# #rm(geomChina)

# Colombia"

geomColombia <- st_read("countries/data/orig_geom/geomColombia.geojson")

geomColombia <- geomColombia%>%
  mutate(
    country_name = "Colombia",
    NOMBRE_DPT = str_to_title(NOMBRE_DPT)
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = DPTO,
    district_name = NOMBRE_DPT
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomColombia%>%
  st_write("countries/data/temp_geom/geomColombia.geojson")
# #rm(geomColombia)

# Cuba"

geomCuba <- st_read("countries/data/orig_geom/geomCuba.geojson")

geomCuba <- geomCuba%>%
  mutate(
    country_name = "Cuba",
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = province_id,
    district_name = province
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))
# Make Cuba valid
geomCuba <- geomCuba%>%
  st_transform(32617)%>%
  st_make_valid()%>%
  st_transform(4326)%>%
  st_make_valid()

geomCuba%>%
  st_write("countries/data/temp_geom/geomCuba.geojson")
# #rm(geomCuba)

# Czechia"

geomCzechia <- st_read("countries/data/orig_geom/geomCzechia.geojson")

geomCzechia <- geomCzechia%>%
  mutate(
    country_name = "Czechia",
    district_code = row_number()
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code,
    district_name = name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomCzechia%>%
  st_write("countries/data/temp_geom/geomCzechia.geojson")
#rm(geomCzechia)

# Denmark"

geomDenmark <- st_read("countries/data/orig_geom/geomDenmark.geojson")

geomDenmark <- geomDenmark%>%
  st_zm()%>%
  group_by(name)%>%
  summarise()%>%
  st_cast("MULTIPOLYGON")%>%
  mutate(
    country_name = "Denmark",
    district_code = row_number()
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code,
    district_name = name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomDenmark%>%
  st_write("countries/data/temp_geom/geomDenmark.geojson")
#rm(geomDenmark)

# Europe"

geomEurope <- st_read("countries/data/orig_geom/geomEurope.geojson")
namesEurope <-  vroom::vroom("countries/data/namesEurope.csv")

##Remove countries
#Turkmenistan - no testing performed. Hence no cases found.
#Countries which we have higher resolution maps
CountriesCovered = c("Italy","Switzerland","Ireland","United Kingdom","Austria","France","Czech Republic","Spain","Denmark","Sweden","Netherlands","Germany","Norway","Belgium","Liechtenstein")

geomEurope <- geomEurope%>%
  inner_join(namesEurope, by = "UID")%>%
  mutate(CountryName=case_when(
    CountryName == "Moldova" ~ "Republic of Moldova",
    CountryName == "Russian Fed." ~ "Russian Federation",
    CountryName == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    TRUE~CountryName
  ))%>%
  left_join(
    m49, by = c("CountryName" = "CountryorArea")
  )%>%
  mutate(
    CountryName=case_when(
      CountryName == "Republic of Moldova"~"Moldova",
      CountryName == "United Kingdom of Great Britain and Northern Ireland"~"United Kingdom",
      CountryName == "Russian Federation" ~ "Russia",
      TRUE~CountryName
    ),
    rem = case_when(
      Region %in% c("Isle of Man","Guernsey","Jersey") ~ 0,
      CountryName %in% c(CountriesCovered, "Turkmenistan") ~ 1,
      TRUE ~ 0
    )
  )%>%
  filter(rem == 0)%>%
  select(
    m49code = M49Code,
    country_name = CountryName,
    district_code = UID,
    district_name = Region
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))
# Make Europe valid
geomEurope <- geomEurope%>%
  st_transform("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")%>% # ESRI 102014: Europe Lambert Conformal Conic
  st_make_valid()%>%
  st_transform(4326)

geomEurope%>%
  st_write("countries/data/temp_geom/geomEurope.geojson")
#rm(geomEurope)
rm(namesEurope)

# France"

geomFrance <- st_read("countries/data/orig_geom/geomFrance.geojson")

geomFrance <- geomFrance%>%
  mutate(
    country_name = "France"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = code,
    district_name = nom
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomFrance%>%
  st_write("countries/data/temp_geom/geomFrance.geojson")
#rm(geomFrance)

# Germany"

geomGermany <- st_read("countries/data/orig_geom/geomGermany.geojson")

geomGermany <- geomGermany%>%
  mutate(
    country_name = "Germany",
    district_code = row_number()
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code,
    district_name = county
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomGermany%>%
  st_write("countries/data/temp_geom/geomGermany.geojson")
#rm(geomGermany)

# India"

geomIndia <- st_read("countries/data/orig_geom/geomIndia.geojson")

geomIndia <- geomIndia%>%
  mutate(
    country_name = "India",
    district_code = row_number()
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code,
    district_name = NAME_1
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomIndia%>%
  st_write("countries/data/temp_geom/geomIndia.geojson")
#rm(geomIndia)

# Indonesia"

geomIndonesia <- st_read("countries/data/orig_geom/geomIndonesia.geojson")

geomIndonesia <- geomIndonesia%>%
  mutate(
    country_name = "Indonesia",
    district_code = row_number()
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code,
    district_name = NAME_1
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomIndonesia%>%
  st_write("countries/data/temp_geom/geomIndonesia.geojson")
#rm(geomIndonesia)

# Ireland"

geomIreland <- st_read("countries/data/orig_geom/geomIreland.geojson")

geomIreland <- geomIreland%>%
  mutate(
    country_name = "Ireland"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    region_name = PROVINCE,
    district_code = CO_ID,
    district_name = id
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))
# Make Ireland valid
geomIreland <- geomIreland%>%
  st_transform(23029)%>%
  st_make_valid()%>%
  st_transform(4326)

geomIreland%>%
  st_write("countries/data/temp_geom/geomIreland.geojson")
#rm(geomIreland)

# Italy"

geomItaly <- st_read("countries/data/orig_geom/geomItaly.geojson")

geomItaly <- geomItaly%>%
  mutate(
    country_name = "Italy"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    region_name = reg_name,
    region_code = reg_istat_code,
    district_code = prov_istat_code,
    district_name = prov_name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomItaly%>%
  st_write("countries/data/temp_geom/geomItaly.geojson")
#rm(geomItaly)

# Japan"

geomJapan <- st_read("countries/data/orig_geom/geomJapan.geojson")

geomJapan <- geomJapan%>%
  mutate(
    country_name = "Japan",
    district_code = row_number()
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code,
    district_name = NAME_1
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

# Make Japan valid
geomJapan <- geomJapan%>%
  st_transform(3100)%>%
  st_make_valid()%>%
  st_transform(4326)

geomJapan%>%
  st_write("countries/data/temp_geom/geomJapan.geojson")
#rm(geomJapan)

# Malaysia"

geomMalaysia <- st_read("countries/data/orig_geom/geomMalaysia.geojson")

geomMalaysia <- geomMalaysia%>%
  mutate(
    country_name = "Malaysia"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = id,
    district_name = Name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomMalaysia%>%
  st_write("countries/data/temp_geom/geomMalaysia.geojson")
#rm(geomMalaysia)

# Mexico"

geomMexico <- st_read("countries/data/orig_geom/geomMexico.geojson")

geomMexico <- geomMexico%>%
  mutate(
    country_name = "Mexico",
    region_code = str_sub(CVEGEO, 1, 2)
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    region_code,
    region_name = estado,
    district_code = CVEGEO,
    district_name = NOMGEO
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

# Make Mexico valid
geomMexico <- geomMexico%>%
  st_transform(6372)%>%
  st_make_valid()%>%
  st_transform(4326)

geomMexico%>%
  st_write("countries/data/temp_geom/geomMexico.geojson")
#rm(geomMexico)

# Mozambique"

geomMozambique <- st_read("countries/data/orig_geom/geomMozambique.geojson")

geomMozambique <- geomMozambique%>%
  mutate(
    country_name = "Mozambique"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = ID_1,
    district_name = NAME_1
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomMozambique%>%
  st_write("countries/data/temp_geom/geomMozambique.geojson")
#rm(geomMozambique)

# Netherlands"

geomNetherlands <- st_read("countries/data/orig_geom/geomNetherlands.geojson")

geomNetherlands%>%
  st_drop_geometry()%>%
  write_csv("countries/data/netherlands_pop.csv")

geomNetherlands <- geomNetherlands%>%
  mutate(
    country_name = "Netherlands"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    region_code = Provinciecode,
    region_name = Provincie,
    district_code = Code,
    district_name = Gemeentenaam
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomNetherlands%>%
  st_write("countries/data/temp_geom/geomNetherlands.geojson")
#rm(geomNetherlands)

# Nigeria"

geomNigeria <- st_read("countries/data/orig_geom/geomNigeria.geojson")

geomNigeria <- geomNigeria%>%
  mutate(
    country_name = "Nigeria"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = admin1Pcod,
    district_name = admin1Name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomNigeria%>%
  st_write("countries/data/temp_geom/geomNigeria.geojson")
#rm(geomNigeria)

# Norway"

geomNorway <- st_read("countries/data/orig_geom/geomNorway.geojson") 
## this geometry is missing Svalbard and Ukjent
norw_kommune <- vroom('https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality.csv')%>%
  select(kommune_no, kommune_name, fylke_no, fylke_name)%>%
  distinct()

geomNorway <- geomNorway%>%
  group_by(kommunenummer)%>%
  summarise()%>%
  st_cast("MULTIPOLYGON")%>%
  mutate(
    country_name = "Norway",
    kommune_no = str_pad(kommunenummer, pad = "0", side = "left", width = 4)
  )%>%
  left_join(
    norw_kommune
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    region_code = fylke_no,
    region_name = fylke_name,
    district_code = kommune_no,
    district_name = kommune_name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomNorway%>%
  st_write("countries/data/temp_geom/geomNorway.geojson")
#rm(geomNorway)
rm(norw_kommune)

# NZ"

geomNZ <- st_read("countries/data/orig_geom/geomNZ.geojson")

geomNZ <- geomNZ%>%
  mutate(
    country_name = "New Zealand"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = DHB2015_Co,
    district_name = DHB2015_Na
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomNZ%>%
  st_write("countries/data/temp_geom/geomNZ.geojson")
#rm(geomNZ)

# Peru"

geomPeru <- st_read("countries/data/orig_geom/geomPeru.geojson")

geomPeru <- geomPeru%>%
  mutate(
    country_name = "Peru",
    NOMBDEP = str_replace_all(
      str_to_title(NOMBDEP),
      "\\sDe\\s",
      " de "
    )
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = FIRST_IDDP,
    district_name = NOMBDEP
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomPeru%>%
  st_write("countries/data/temp_geom/geomPeru.geojson")
#rm(geomPeru)

# Philippines"

geomPhilippines <- st_read("countries/data/orig_geom/geomPhilippines.geojson")
namesPhilippines <- vroom("countries/data/namesPhilippines.csv")%>%
  select(starts_with("ADM2"),starts_with("ADM1"))%>%
  distinct()

geomPhilippines <- geomPhilippines%>%
  mutate(
    country_name = "Philippines"
  )%>%
  left_join(namesPhilippines
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    region_code = ADM1_PCODE,
    region_name = ADM1_EN,
    district_code = ADM2_PCODE,
    district_name = ADM2_EN
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomPhilippines%>%
  st_write("countries/data/temp_geom/geomPhilippines.geojson")
#rm(geomPhilippines)
rm(namesPhilippines)

# SaudiArabia"

geomSaudiArabia <- st_read("countries/data/orig_geom/geomSaudiArabia.geojson")

geomSaudiArabia <- geomSaudiArabia%>%
  mutate(
    country_name = "Saudi Arabia"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = REG_CODE,
    district_name = region_name_en
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))%>%
  st_make_valid() # Make Saudi Arabia valid

geomSaudiArabia%>%
  st_write("countries/data/temp_geom/geomSaudiArabia.geojson")
#rm(geomSaudiArabia)

# SouthAfrica"

geomSouthAfrica <- st_read("countries/data/orig_geom/geomSouthAfrica.geojson")

geomSouthAfrica <- geomSouthAfrica%>%
  mutate(
    country_name = "South Africa"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = PROVINCE
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomSouthAfrica%>%
  st_write("countries/data/temp_geom/geomSouthAfrica.geojson")
#rm(geomSouthAfrica)

# Spain"

geomSpain <- st_read("countries/data/orig_geom/geomSpain.geojson")

geomSpain <- geomSpain%>%
  mutate(
    country_name = "Spain"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    region_code = cod_ccaa,
    district_code = cod_prov,
    district_name = name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))%>%
  st_make_valid()# Make Spain valid

geomSpain%>%
  st_write("countries/data/temp_geom/geomSpain.geojson")
#rm(geomSpain)

# Sweden"

geomSweden <- st_read("countries/data/orig_geom/geomSweden.geojson")

geomSweden <- geomSweden%>%
  mutate(
    country_name = "Sweden"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = cartodb_id,
    district_name = name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomSweden%>%
  st_write("countries/data/temp_geom/geomSweden.geojson")
#rm(geomSweden)

# SwitzerlandLiechtenstein"

geomSwitzerlandLiechtenstein <- st_read("countries/data/orig_geom/geomSwitzerlandLiechtenstein.geojson")

geomSwitzerlandLiechtenstein <- geomSwitzerlandLiechtenstein%>%
  mutate(
    country_name = case_when(
      name == "Liechtenstein" ~ "Liechtenstein",
      TRUE ~ "Switzerland"
    ))%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    district_code = id,
    district_name = name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

# Make Switzerland and Liechtenstein valid
geomSwitzerlandLiechtenstein <- geomSwitzerlandLiechtenstein%>%
  st_transform(21781)%>%
  st_make_valid()%>%
  st_transform(4326)

geomSwitzerlandLiechtenstein%>%
  st_write("countries/data/temp_geom/geomSwitzerlandLiechtenstein.geojson")
#rm(geomSwitzerlandLiechtenstein)

# Taiwan"

geomTaiwan <- st_read("countries/data/orig_geom/geomTaiwan.geojson")

geomTaiwan <- geomTaiwan%>%
  mutate(
    country_name = "Taiwan",
    m49code = 158 # Using ISO3166 code bc Taiwan is not in the M49 list
  )%>%
  select(
    m49code,
    country_name,
    district_code = COUNTYSN,
    district_name = COUNTYNAME
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomTaiwan%>%
  st_write("countries/data/temp_geom/geomTaiwan.geojson")
#rm(geomTaiwan)

# Thailand"

geomThailand <- st_read("countries/data/orig_geom/geomThailand.geojson")

geomThailand <- geomThailand%>%
  mutate(
    country_name = "Thailand"
  )%>%
  left_join(
    m49, by = c("country_name" = "CountryorArea")
  )%>%
  select(
    m49code = M49Code,
    country_name,
    region_name = reg_nesdb,
    district_code = pro_code,
    district_name = pro_en
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomThailand%>%
  st_write("countries/data/temp_geom/geomThailand.geojson")
#rm(geomThailand)

# UnitedKingdom"

geomUnitedKingdom <- st_read("countries/data/orig_geom/geomUnitedKingdom.geojson")

geomUnitedKingdom <- geomUnitedKingdom%>%
  mutate(
    country_name = "United Kingdom",
    m49code = m49$M49Code[which(m49$CountryorArea == "United Kingdom of Great Britain and Northern Ireland")]
  )%>%
  select(
    m49code,
    country_name,
    district_code = code,
    district_name = name
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomUnitedKingdom%>%
  st_write("countries/data/temp_geom/geomUnitedKingdom.geojson")
#rm(geomUnitedKingdom)

# UnitedStates"

geomUnitedStates <- st_read("countries/data/orig_geom/geomUnitedStates.geojson")

geomUnitedStates <- geomUnitedStates%>%
  mutate(
    country_name = "United States",
    region_code = str_sub(GEOID, 1, 2),
    m49code = m49$M49Code[which(m49$CountryorArea == "United States of America")]
  )%>%
  select(
    m49code,
    country_name,
    region_code,
    region_name = stname,
    district_code = GEOID,
    district_name = NAME
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

geomAK <- geomUnitedStates%>%
  filter(region_name == "AK")%>%
  st_transform(3467)%>%
  st_make_valid()%>%
  st_transform(4326)

geomCO <- geomUnitedStates%>%
  filter(region_name == "CO")%>%
  st_transform(2773)%>%
  st_make_valid()%>%
  st_transform(4326)

geomID <- geomUnitedStates%>%
  filter(region_name == "ID")%>%
  st_transform(2788)%>%
  st_make_valid()%>%
  st_transform(4326)

geomKS <- geomUnitedStates%>%
  filter(region_name == "KS")%>%
  st_make_valid()

geomNew <- bind_rows(geomAK, geomCO, geomID, geomKS)

for(x in geomNew$district_code){
  st_geometry(geomUnitedStates)[geomUnitedStates$district_code==x] = st_geometry(geomNew)[geomNew$district_code==x]
}

geomUnitedStates%>%
  st_write("countries/data/temp_geom/geomUnitedStates.geojson")
#rm(geomUnitedStates)
rm(list=c("geomAK", "geomCO", "geomID", "geomKS", "geomNew"))

# Venezuela"

geomVenezuela <- st_read("countries/data/orig_geom/geomVenezuela.geojson")

geomVenezuela <- geomVenezuela%>%
  mutate(
    country_name = "Venezuela",
    m49code = m49$M49Code[which(m49$CountryorArea == "Venezuela (Bolivarian Republic of)")]
  )%>%
  select(
    m49code,
    country_name,
    district_code = ID_1,
    district_name = NAME_1
  )%>%
  mutate(across(.cols=ends_with("code"),.fns=as.character))

# Make Venezuela valid
geomVenezuela <- geomVenezuela%>%
  st_transform(2201)%>%
  st_make_valid()%>%
  st_transform(4326)

geomVenezuela%>%
  st_write("countries/data/temp_geom/geomVenezuela.geojson")
#rm(geomVenezuela)


geom_list <- list.files("countries/data/temp_geom/", full.names = T)

geom_list <- str_extract(geom_list[str_which(geom_list,".geojson$")],"[:alpha:]*(?=.geojson)")

geomWorld <- purrr::map_df(geom_list, ~get(.x)%>%
                             mutate(filename=.x)%>%
                             st_collection_extract()%>%
                             st_cast("MULTIPOLYGON")
    )%>%
  st_as_sf()%>%
  tibble::remove_rownames()

st_write(geomWorld, "countries/data/WorldPreSimp/geomWorld_presimp.geojson", delete_dsn = T)
# file.remove(list.files("countries/data/temp_geom", full.names = T))
