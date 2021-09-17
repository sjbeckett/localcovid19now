LoadPhilippines <- function(){
#Covid-19 numbers per municipality as of publication date. RIVM / I & V / EPI. OSIRIS General Infectious Diseases (AIZ). https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/5f6bc429-1596-490e-8618-1ed8fd768427?tab=general

  key <- readChar("drive_api.txt",file.info("drive_api.txt")$size)
  drive_auth_configure(api_key = key)
  drive_api_key()
  
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
    group_by(
      DateRepConf,
      RegionRes,
      ProvRes
    )%>%
    summarise(
      TotalReported = n()
    )
  
data <- read.csv('https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv', sep = ';',fileEncoding = 'UTF-8')
netherlandsData <- data[,c('Date_of_publication','Municipality_code','Municipality_name','Province','Total_reported')]
names(netherlandsData) <- c('Date','Code','Municipality','Province','Cases')
netherlandsData$Date <- as.Date(netherlandsData$Date)
### Municipalities:
municipality <- unique(netherlandsData$Municipality)
getData <- function(code){
  temp <- netherlandsData %>% filter(netherlandsData$Municipality == municipality[code])
  temp$CumSum <- cumsum(temp$Cases)
  today <- temp$Date[length(temp$Date)]
  past_date <- today - 14
  pastData <- temp[temp$Date <= past_date,]
  ### SOME ROWS DO NOT REPORT MUNICIPALITY NAME
  difference <- (temp$CumSum[length(temp$CumSum)] - pastData$CumSum[length(pastData$CumSum)])/14*10
  vec <- data.frame(Municipality = municipality[code], Code = temp$Code[1], Date = today, Difference = difference)
  return(vec)
}

netherlandsTable <- data.frame()
for (i in 1:length(municipality)){
  vec <- getData(i)
  netherlandsTable <- rbind(netherlandsTable,vec)
}

netherlandsTable$Municipality[netherlandsTable$Municipality == "'s-Gravenhage" ] <- "Des Gravenhage"


### Geometry:
#geomNetherlands <- st_read('https://opendata.arcgis.com/datasets/620c2ab925f64ed5979d251ba7753b7f_0.geojson')
# Note that geomNetherlands$Bevolkingsaantal is population size.
geomNetherlands = st_read("countries/data/geom/geomNetherlands.geojson")

netherlandsMap <- inner_join(geomNetherlands, netherlandsTable, by = c("Gemeentecode" = "Code"))
netherlandsMap$RegionName = paste0(netherlandsMap$Municipality,", Netherlands")
netherlandsMap$Country = "Netherlands"
netherlandsMap$DateReport = as.character(netherlandsMap$Date)
netherlandsMap$pInf = netherlandsMap$Difference/netherlandsMap$Bevolkingsaantal
NETHERLANDS_DATA = subset(netherlandsMap,select=c("DateReport","RegionName","Country","pInf","geometry"))

return(NETHERLANDS_DATA)
}