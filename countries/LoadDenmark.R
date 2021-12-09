LoadDenmark <- function(){
  #COVID-19 data from the Statens Serum Institut (SSI):
  #- https://covid19.ssi.dk/overvagningsdata  
  #- https://experience.arcgis.com/experience/aa41b29149f24e20a4007a0c4e13db1d  
  
  #geometry
  #geomDenmark <- st_read('https://raw.githubusercontent.com/magnuslarsen/geoJSON-Danish-municipalities/master/municipalities/municipalities.geojson')
  geomDenmark <-st_read("countries/data/geom/geomDenmark.geojson")
  #name alteration for matching case data
  Name2save1 = geomDenmark$micro_name[42] #"Høje-Taastrup"
  Name2save2 = geomDenmark$micro_name[60] #"Lyngby-Taarbæk"
  Name2save3 = geomDenmark$micro_name[73] #"Ringkøbing-Skjern"
  
  
  #case data
  # 1.)  identify file location from webpages
  webpages<-read_html("https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata")
  #extract the html blocks which are h5 and contain links
  JAM = webpages %>% html_nodes("h5") %>% html_nodes("a")
  #subset this list by looking for items which are in the directory of interest
  INDEX = which(grepl("https://files.ssi.dk/covid19/overvagning/data/",JAM,fixed=TRUE))
  
  #JAM[2] should be the download link -- unless the website changes...Not sure if there is an easy way to double check this is the right code block?
  
  # Should be able to use """ html_elements(webpages,xpath="/html/body/div[2]/div[2]/section[5]/accordions/div[2]/div/div/ul/li[1]/h5/a")%>%html_attr("href") """ to get the link as well, but I haven't tested it for reliability
  
  # extract the text from href attribute for the link. this is faster than the substring method
  DOWNLOADLINK = html_attr(JAM[INDEX[1]], "href")
  DOWNLOADLINK = paste0(DOWNLOADLINK,".zip")  #need to add .zip extension in order for the download/extraction process to perform correctly in R.
  #Have the download link!
  
  # 2.) download and extract data:
  temp <- tempfile() #temporary file for download
  temp2 <- tempdir()#temporary file for extraction
  download.file(DOWNLOADLINK, destfile = temp)
  unzip(zipfile = temp, exdir = temp2)
  DenmarkData  <- vroom(file.path(temp2, "Municipality_cases_time_series.csv"),delim=";")
  unlink(temp)
  unlink(temp2)
  otime <- system.time({
  #calculate case differences per location (file is new cases per day)
  DenmarkCounty <- names(DenmarkData)[2:length(names(DenmarkData))] 
  DenmarkData$SampleDate <- as.Date(DenmarkData$SampleDate)
  
  getDenmarkData <- function(code){
    subdata <- DenmarkData[,c("SampleDate",DenmarkCounty[code])]
    subdata$CumCases <- cumsum(subdata[,DenmarkCounty[code]])
    x <- length(subdata$SampleDate)
    difference <- ((subdata[x,'CumCases'] - subdata[x-14,'CumCases'])*10/14)
    vec <- data.frame(Municipality = DenmarkCounty[code], Date = subdata$SampleDate[x], Difference = pull(difference))
    return(vec)
  }
  
  dataTable <- data.frame(Municipality = as.character(), Date = as.character(), Difference = as.numeric())
  for (ii in 1:length(DenmarkCounty)){
    vec <- getDenmarkData(ii)
    dataTable <- rbind(dataTable,vec)
  }
  dataTable <- dataTable %>% mutate(Municipality = as.character(Municipality), Date = as.Date(Date))
     
 
  # adjust some municipalities' names so that they match with population file

  dataTable$Municipality[which(dataTable$Municipality == "Faaborg.Midtfyn")] <-  "Faaborg-Midtfyn"
  dataTable$Municipality[which(dataTable$Municipality == "Ikast.Brande")] <-  "Ikast-Brande"
  
  dataTable$Municipality[which(dataTable$Municipality == sort(DenmarkCounty)[42])] = Name2save1
  dataTable$Municipality[which(dataTable$Municipality == sort(DenmarkCounty)[60])] = Name2save2
  dataTable$Municipality[which(dataTable$Municipality == sort(DenmarkCounty)[74])] = Name2save3
  
  })
  #test
  ntime <- system.time({
  curdate <- DenmarkData$SampleDate%>%sort()%>%last()
  
  dataTable2 <- DenmarkData%>%
    pivot_longer(
      cols = -SampleDate,
      names_to = "Municipality",
      values_to = "Cases"
    )%>%
    group_by(Municipality)%>%
    mutate(
      CumCases = cumsum(Cases)
    )%>%
    ungroup()%>%
    filter(SampleDate %in% c(curdate, curdate-14))%>%
    pivot_wider(
      id_cols = Municipality,
      names_from = SampleDate,
      values_from = CumCases,
      names_prefix = "d"
      )%>%
    mutate(
      Difference = (.data[[paste0("d",curdate)]]-.data[[paste0("d",(curdate-14))]])*10/14,
      Date=curdate
    )%>%
    select(Date, everything(), -starts_with("d",ignore.case = F))
  })
otime
ntime
 
  #population
  DenmarkPop <- vroom("countries/data/denmark_pop.csv") ## get from Statistics Denmark: https://www.statbank.dk/statbank5a/SelectVarVal/saveselections.asp
  names(DenmarkPop) <- c("Municipality",'Population')
  # make the population column as numeric
  DenmarkPop$Population <- as.numeric(gsub(" ", "", DenmarkPop$Population))
  
  #integrate datasets 
  Denmarkdf <- inner_join(dataTable, DenmarkPop, by = "Municipality")
  Denmarkdf$Municipality[which(Denmarkdf$Municipality == 'Nordfyns')] <- "Nordfyn"
  Denmarkdf$Municipality[which(Denmarkdf$Municipality == 'Vesthimmerlands')] <- "Vesthimmerland"
    
  DenmarkMap <- inner_join(geomDenmark, Denmarkdf, by = c("name" = "Municipality"))
 
  DenmarkMap$RegionName = paste(DenmarkMap$micro_name,DenmarkMap$country_name, )
  DenmarkMap$Country = "Denmark"
  DenmarkMap$DateReport = as.character(DenmarkMap$Date) 
  DenmarkMap$pInf = DenmarkMap$Difference/as.numeric(DenmarkMap$Population)
  DENMARK_DATA = subset(DenmarkMap,select=c("DateReport","RegionName","Country","pInf","geometry"))
  
return(DENMARK_DATA)
}
