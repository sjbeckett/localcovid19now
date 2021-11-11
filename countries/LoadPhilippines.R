LoadPhilippines <- function(){
#Republic of Philippines Department of Health: https://doh.gov.ph/covid19tracker
 
  
  url1 <- "bit.ly/DataDropPH"
  req1 <- GET(url1)
  folder_dr <- drive_ls(str_extract(req1$url,"[:graph:]*(?=\\?)"))
  drive_download(
    file = paste("https://drive.google.com/file/d",folder_dr["id"]%>%pull,sep = "/"),
    overwrite = TRUE
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
    fread(showProgress = TRUE, select = c("DateRepConf","ProvRes","RegionRes"), encoding = "UTF-8"
      # text = .,
    )
	
	
	  col_types = cols(DateSpecimen = col_date(format = "%Y-%m-%d"),
                       DateResultRelease = col_date(format = "%Y-%m-%d"),
                       DateRepConf = col_date(format = "%Y-%m-%d"),
                       DateDied = col_date(format = "%Y-%m-%d"),
                       DateRecover = col_date(format = "%Y-%m-%d"),
                       DateOnset = col_date(format = "%Y-%m-%d"))
  
  philippinesData <- case_details%>%
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
      RegionRes,
      ProvRes
    )%>%
    summarise(
      TotalReported = n()
    )%>%
    ungroup()
  
names(philippinesData) <- c('Date','Region','Province','Cases')
### Population
philippinesPop <- read_csv("countries/data/philippinesPop2015.csv")



### Municipalities:
province <- unique(philippinesData$Province)
province <- province[is.na(province)==F]
getData <- function(code){
  temp <- philippinesData %>% filter(philippinesData$Province == province[code])
  temp$CumSum <- cumsum(temp$Cases)
  today <- temp$Date[length(temp$Date)]
  past_date <- as.Date(today) - 14
  pastData <- temp[temp$Date <= past_date,]
  difference <- (temp$CumSum[length(temp$CumSum)] - pastData$CumSum[length(pastData$CumSum)])/14*10
  vec <- data.frame(Province = province[code], Date = as.character(today), Difference = difference)
  return(vec)
}

philippinesTable <- data.frame()
for (i in 1:length(province)){
  vec <- getData(i)
  philippinesTable <- bind_rows(philippinesTable,vec)
}

### Geometry:
geomPhilippines = st_read("countries/data/geom/geomPhilippines.geojson")

geomPhilippines <- geomPhilippines%>%
  left_join(
    philippinesPop,
    by = c("ADM2_EN" = "Location")
  )

philippinesMap <- inner_join(geomPhilippines, philippinesTable, by = c("ADM2_EN" = "Province"))
philippinesMap$RegionName = paste0(philippinesMap$ADM2_EN,", Philippines")
philippinesMap$Country = "Philippines"
philippinesMap$DateReport = as.character(philippinesMap$Date)
philippinesMap$pInf = philippinesMap$Difference/philippinesMap$Pop2015
PHILIPPINES_DATA = subset(philippinesMap,select=c("DateReport","RegionName","Country","pInf","geometry"))

return(PHILIPPINES_DATA)
}