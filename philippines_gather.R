list.of.packages <- c("sf","raster", "ggmap", "maps","OpenStreetMap","pdftools","googledrive","httr","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(c(list.of.packages, "rvest","lubridate"),FUN = require,character.only = TRUE))

#"196966001895-g2h583ktjt6uq466p6ijlg9taupkmdpb.apps.googleusercontent.com"

# drive_auth_configure(path = "C:/Users/Freyja/OneDrive - Georgia Institute of Technology/CDC GRA/cdc-gra/OAuth/philippines_oauth.json")
key <- readChar("drive_api.txt",file.info("drive_api.txt")$size)
drive_auth_configure(path = "phil_oauth.json")


url1 <- "bit.ly/DataDropPH"
req1 <- GET(url1)
folder_dr <- drive_ls(str_extract(req1$url,"[:graph:]*(?=\\?)"))
drive_download(
  file = paste("https://drive.google.com/file/d",folder_dr["id"]%>%pull,sep = "/")
  )

readme_pdf <- dir()%>%
  .[str_detect(dir(),"READ ME FIRST")]%>%
  as_tibble()%>%
  mutate(file_date= str_extract(value,"[:digit:]{2}_[:digit:]{2}")%>%
  paste(.,"2021",sep="_")%>%
  mdy()
  )%>%
  filter(
    file_date == max(file_date)
  )%>%
  .$"value"%>%
  pdf_text()

data_link <- readme_pdf%>%
  str_extract("(?<=bit.ly)[:graph:]*")%>%
  .[!is.na(.)]

url2 <- paste0("https://bit.ly",data_link)

req2 <- GET(url2)
folder_data <- drive_ls(str_extract(req2$url,"[:graph:]*(?=\\?)"))

caseinfo_id <- folder_data%>%
  filter(str_detect(name,"04 Case Information.csv"))%>%
  select(id)%>%
  pull

# case_download <- drive_download(
#     file=paste("https://drive.google.com/file/d",caseinfo_id,sep = "/"),
#     path = paste(tempdir(),"philippinescases.csv",sep="/")
#     )

case_details <- drive_read_string(
  file=paste("https://drive.google.com/file/d",caseinfo_id,sep = "/")
)%>%
  read_csv(
    # text = .,
    col_types = cols(DateSpecimen = col_date(format = "%Y-%m-%d"),
                     DateResultRelease = col_date(format = "%Y-%m-%d"),
                     DateRepConf = col_date(format = "%Y-%m-%d"),
                     DateDied = col_date(format = "%Y-%m-%d"),
                     DateRecover = col_date(format = "%Y-%m-%d"),
                     DateOnset = col_date(format = "%Y-%m-%d"))
  )

data <- case_details%>%
  mutate(
    ProvRes = case_when(
      str_detect(ProvRes, "\\(") == T ~ str_to_title(str_replace(ProvRes, "\\s\\(([:graph:]*[:blank:]?)*\\)","")),
      TRUE ~ str_to_title(ProvRes)
    ),
    ProvRes = str_replace_all(
      ProvRes,
      "\\sDel\\s",
      " del "
    ),
    ProvRes = str_replace_all(
      ProvRes,
      "\\sDe\\s",
      " de "
    ),
    ProvRes = str_replace_all(
      ProvRes,
      "\\sOf\\s",
      " of "
    ),
    ProvRes = str_replace_all(
      ProvRes,
      "^Ncr$",
      "NCR"
    )
  )%>%
  group_by(
    DateRepConf,
    ProvRes
  )%>%
  summarise(
    TotalReported = n()
  )%>%
  filter(
    DateRepConf == "2021-09-17"
  )


temp <- tempfile()
download.file("https://data.humdata.org/dataset/caf116df-f984-4deb-85ca-41b349d3f313/resource/12457689-6a86-4474-8032-5ca9464d38a8/download/phl_adm_psa_namria_20200529_shp.zip", temp)
adm2 <- unzip(temp, list = T)
unzip(temp, adm2$Name[grep("phl_admbnda_adm2_psa_namria_20200529",x=adm2$Name)], exdir = tempdir())
geomPhilippines_raw <- st_read(paste(tempdir(),"phl_admbnda_adm2_psa_namria_20200529.shp",sep = "\\"))
unlink(temp)

# geom1k <- geomPhilippines_raw%>%
#   st_transform(3121)%>%
#   st_simplify(dTolerance = 1000)%>%
#   st_transform(4326)
# 
# geom2k <- geomPhilippines_raw%>%
#   st_transform(3121)%>%
#   st_simplify(dTolerance = 2000)%>%
#   st_transform(4326)
# 
# geom3k <- geomPhilippines_raw%>%
#   st_transform(3121)%>%
#   st_simplify(dTolerance = 2000)%>%
#   st_transform(4326)
# 
# ggplot()+
#   geom_sf(data = geom1k, fill = NA, color = 'red')+
#   geom_sf(data = geom2k, fill = NA, color = 'blue')+
#   geom_sf(data = geom3k, fill = NA, color = 'green')
# 
# geom500 <- 
# 
# ggplot()+
#   geom_sf(data = geomPhilippines_raw, fill = NA, color = 'black')+
#   geom_sf(data = geom500, fill = NA, color = 'red')

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

phil_case <- geomPhilippines%>%
  left_join(
    data, by = c("ADM2_EN"="ProvRes")
  )

ggplot(phil_case)+
  geom_sf(
    aes(fill = TotalReported)
  )

phil_case%>%
  st_drop_geometry%>%
  filter(is.na(TotalReported))%>%
  select(ADM2_EN)%>%
  pull

data%>%
  select(ProvRes)%>%
  pull

# Test
geomPhilippines%>%
  st_drop_geometry()%>%
  left_join(philippinesPop,by = c("ADM2_EN" = "Location"))%>%
  filter(is.na(Pop2015)==T)
