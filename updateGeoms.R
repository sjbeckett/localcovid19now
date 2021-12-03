source("librariesMinimal.R")
# geojson_list <- list.files("countries/data/geom/", full.names = T)
# 
# geoNames <-  list()
# 
# extract_altinfo <- function(geojson, namelist){
#   
#   if(!str_detect(geojson,".geojson$")){
#     cat("\nnot geojson\n")
#     }else{
#   fname <- str_sub(str_extract(geojson,"[:alpha:]*(?=.geojson)"), 5L, -1L)
#   cat("\n", fname,"\n")
#   geo <- st_read(geojson)
#   namelist[fname] <- list(names(geo))
#   return(namelist)
#     }
# }
# 
# geoNames <- purrr::map(geojson_list,~extract_altinfo(.x, geoNames))

# Fields will be as follows:
## id
## m49_code
## county_name
## region_code
## region_name
## district_code
## district_name
## geometry
m49 <- vroom("UNSD_m49.csv")
m49 <- m49%>%
  rename_with(
    .fn = \(x) str_replace_all(x, "[\\s/-]","")
  )

# Afghanistan

geomAfghanistan <- st_read("countries/data/geom/geomAfghanistan.geojson")

## Afghanistan has no supplemental variables in this data set
# afghanistanGeom%>%
#   st_drop_geometry()%>%
#   write_csv(
#     "countries/data/misc/afghanistanPop.csv"
#   )

geomAfghanistan <- geomAfghanistan%>%
  mutate(
    country_name = "Afghanistan",
    m49code = m49$M49Code[which(m49$CountryorArea == "Afghanistan")]
  )%>%
  select(
    m49code,
    country_name,
    district_code = id,
    district_name = name
  )

geomAfghanistan%>%
  st_write("countries/data/temp_geom/geomAfghanistan.geojson")
rm(geomAfghanistan)

# Algeria
geomAlgeria <- st_read("countries/data/geom/geomAlgeria.geojson")

geomAlgeria <- geomAlgeria%>%
  mutate(
    country_name = "Algeria",
    m49code = m49$M49Code[which(m49$CountryorArea == "Algeria")]
  )%>%
  select(
    m49code,
    country_name,
    district_code = ID,
    district_name = NAME
  )

geomAlgeria%>%
  st_write("countries/data/temp_geom/geomAlgeria.geojson")
rm(geomAlgeria)

# Argentina
geomArgentina <- st_read("countries/data/geom/geomArgentina.geojson")

geomArgentina <- geomArgentina%>%
  mutate(
    country_name = "Argentina",
    m49code = m49$M49Code[which(m49$CountryorArea == "Argentina")]
  )%>%
  select(
    m49code,
    country_name,
    district_code = ID_1,
    district_name = NAME_1
  )

geomArgentina%>%
  st_write("countries/data/temp_geom/geomArgentina.geojson")
rm(geomArgentina)

# Australia 
geomAustralia <- st_read("countries/data/geom/geomAustralia.geojson")

geomAustralia <- geomAustralia%>%
  mutate(
    country_name = "Australia",
    m49code = m49$M49Code[which(m49$CountryorArea == "Australia")]
  )%>%
  select(
    m49code,
    country_name,
    district_code = STATE_CODE,
    district_name = STATE_NAME
  )

geomAustralia%>%
  st_write("countries/data/temp_geom/geomAustralia.geojson")
rm(geomAustralia)

# Austria
geomAustria <- st_read("countries/data/geom/geomAustria.geojson")

geomAustria <- geomAustria%>%
  mutate(
    country_name = "Austria",
    m49code = m49$M49Code[which(m49$CountryorArea == "Austria")]
  )%>%
  select(
    m49code,
    country_name,
    district_code = iso,
    district_name = name
  )

geomAustria%>%
  st_write("countries/data/temp_geom/geomAustria.geojson")
rm(geomAustria)

# Belgium
geomBelgium <- st_read("countries/data/geom/geomBelgium.geojson")

geomBelgium <- geomBelgium%>%
  mutate(
    country_name = "Belgium",
    m49code = m49$M49Code[which(m49$CountryorArea == "Belgium")],
    district_code = row_number() # I'm assigning IDs based on row number, but we could go back and do country subdivision ISOs
  )%>%
  select(
    m49code,
    country_name,
    district_code,
    district_name = NameDUT
  )

geomBelgium%>%
  st_write("countries/data/temp_geom/geomBelgium.geojson")
rm(geomBelgium)


# Brazil"

geomBrazil <- st_read("countries/data/geom/geomBrazil.geojson")

geomBrazil <- geomBrazil%>%
  mutate(
    country_name = "Brazil",
    m49code = m49$M49Code[which(m49$CountryorArea == "Brazil")]
  )%>%
  select(
    m49code,
    country_name,
    region_code = regiao_id,
    district_code = codigo_ibg,
    district_name = nome
  )

geomBrazil%>%
  st_write("countries/data/temp_geom/geomBrazil.geojson")
rm(geomBrazil)


# Canada"

geomCanada <- st_read("countries/data/geom/geomCanada.geojson")

geomCanada%>%
  st_drop_geometry()%>%
  write_csv("countries/data/miscCanada.csv")

geomCanada <- geomCanada%>%
  mutate(
    country_name = "Canada",
    m49code = m49$M49Code[which(m49$CountryorArea == "Canada")]
  )%>%
  select(
    m49code,
    country_name,
    district_code = HR_UID,
    district_name = ENGNAME
  )

geomCanada%>%
  st_write("countries/data/temp_geom/geomCanada.geojson")
rm(geomCanada)

# Chile"

geomChile <- st_read("countries/data/geom/geomChile.geojson")

geomChile <- geomChile%>%
  mutate(
    country_name = "Chile",
    m49code = m49$M49Code[which(m49$CountryorArea == "Chile")]
  )%>%
  select(
    m49code,
    country_name,
    district_code = codregion,
    district_name = matchName
  )

geomChile%>%
  st_write("countries/data/temp_geom/geomChile.geojson")
rm(geomChile)

# China"

# Colombia"

# Cuba"

# Czechia"

# Denmark"

# Europe"

# France"

# Germany"

# India"

# Indonesia"

# Ireland"

# Italy"

# Japan"

# Malaysia"

# Mexico"

# Mozambique"

# Netherlands"

# Nigeria"

# Norway"

# NZ"

# Peru"

# Philippines"

# SaudiArabia"

# SouthAfrica"

# Spain"

# Sweden"

# SwitzerlandLiechtenstein"

# Taiwan"

# Thailand"

# UnitedKingdom"

# UnitedStates"

# Venezuela"
