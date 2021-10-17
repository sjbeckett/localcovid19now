# philippinesExternal

#Population
# Data from https://openstat.psa.gov.ph/PXWeb/pxweb/en/DB/DB__1A__PO/1001A6DTPG0.px/?rxid=ab9e33b0-a5cc-4180-ad72-19538f4b2b0a
philippinesPop <- philippinesPopr%>%
  mutate(
    Location = str_replace_all(Location, "\\.", ""),
    Location = case_when(
      str_detect(Location, "NCR") ~ "NCR",
      Location == "Compostela Valley" ~ "Davao de Oro",
      TRUE ~ Location
    ),
    Location = str_replace_all(Location, "�", "ñ"),
    Location = str_replace_all(
      Location,
      "\\sDel\\s",
      " del "
    ),
    Location = str_replace_all(
      Location,
      "\\sDe\\s",
      " de "
    ),
    Location = str_replace_all(
      Location,
      "\\sOf\\s",
      " of "
    )
  )

cityIn <- philippinesPop%>%
  select(-Pop2015)%>%
  filter(
    str_detect(Location, "excluding") == T
  )%>%
  separate(
    Location,
    c("Location","City"),
    sep = "\\s\\(excluding\\s"
  )%>%
  mutate(
    City = str_replace_all(City, "\\)", "")
  )%>%
  filter(Location != "Cebu")%>%
  bind_rows(
    tibble(
      Location = c("Cebu"),
      City = c("Cebu City", "Lapu-Lapu City", "Mandaue City")
    )
  )

provCity <- function(x){
  if(x %in% cityIn$City){
    return(cityIn$Location[str_which(cityIn$City, x)])
  }else{
    return(x)
  }
}

philippinesPop2 <- philippinesPop%>%
  mutate(
    Location = case_when(
      str_detect(Location, "\\(") ~ str_extract(Location, ".*(?=\\()"),
      TRUE ~ Location
    ))%>%
  rowwise()%>%
  mutate(
    groupvar = provCity(Location),
    across(
      .cols = c("Location", "groupvar"),
      .fns = \(x) str_trim(x, side = "both")
    )
  )%>%
  ungroup()%>%
  group_by(groupvar)%>%
  summarise(
    Pop2015 = sum(Pop2015)
  )%>%
  ungroup(
  )%>%
  rename(Location = groupvar)%>%
  bind_rows(
    philippinesPop
    # tibble(
    #   groupvar = c("Cotabato City","City of Isabela"),
    #   Pop2015 = c(299438, 112788)
    # )
  )%>%
  distinct()

philippinesPop2%>%
  write_csv("countries/data/philippinesPop2015.csv")

# geoJson
temp <- tempfile()
download.file("https://data.humdata.org/dataset/caf116df-f984-4deb-85ca-41b349d3f313/resource/12457689-6a86-4474-8032-5ca9464d38a8/download/phl_adm_psa_namria_20200529_shp.zip", temp)
adm2 <- unzip(temp, list = T)
unzip(temp, adm2$Name[grep("phl_admbnda_adm2_psa_namria_20200529",x=adm2$Name)], exdir = tempdir())
geomPhilippines_raw <- st_read(paste(tempdir(),"phl_admbnda_adm2_psa_namria_20200529.shp",sep = "\\"))
unlink(temp)

geomPhilippines <- geomPhilippines_raw%>%
  st_transform(3121)%>%
  st_simplify(dTolerance = 500)%>%
  st_transform(4326)%>%
  mutate(
    ADM2_EN = case_when(
      str_detect(ADM2_EN, "^NCR") == T ~ "NCR",
      ADM2_EN == "Compostela Valley" ~ "Davao de Oro",
      TRUE ~ ADM2_EN
    )
  )%>%
  group_by(ADM2_EN, ADM1_EN)%>%
  summarise()%>%
  ungroup()

geomPhilippines%>%
  st_write("countries/data/geom/geomPhilippines.geojson")
