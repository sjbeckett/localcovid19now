LoadSweden <- function() {
#Swedish COVID-19 National Statistics from Folkh?lsomyndigheten: https://experience.arcgis.com/experience/09f821667ce64bf7be6f9f87457ed9aa/page/page_0/

  temp <- tempfile()
  download.file(url = "https://fohm.maps.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data", destfile = temp, mode="wb")
  swedenResource <- as.data.frame(readxl::read_excel(temp,col_names =TRUE))
  unlink(temp)
  names(swedenResource)[1] <- 'date'
  swedenResource$date <- as.Date(swedenResource$date)
  SwedenCounty <- names(swedenResource)[3:length(names(swedenResource))]
  SwedenCounty[SwedenCounty == "Jämtland_Härjedalen"] <- "Jämtland"
  SwedenCounty[SwedenCounty == "Sörmland"] <- "Södermanland"
  SwedenCounty[SwedenCounty == "Västra_Götaland"] <- "Västra Götaland"
  names(swedenResource) = c(names(swedenResource)[1:2],SwedenCounty)

  data = swedenResource %>%
    pivot_longer(3:23, names_to="County", values_to="cases") %>%
    select(-Totalt_antal_fall) %>%
    arrange(desc(date))

  #geometry
  #geom <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/sweden-counties.geojson")
  geom<-st_read("countries/data/geom/geomSweden.geojson")

  #population
  #pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/sweden_pop.csv", encoding = 'UTF-8')
  pop <-read.csv("countries/data/Sweden_pop.csv", encoding="UTF-8")


  data_cur <<- data %>%
    group_by(County) %>%
    summarise(County = first(County), cases = sum(cases), date = first(date)) %>%
    as.data.frame()
  past_date <- data_cur$date[1] - 14
  data_past <- data %>%
    filter(date <= past_date) %>%
    group_by(County) %>%
    summarise(County = first(County), cases = sum(cases), date = first(date)) %>%
    as.data.frame()
  data_join <<- data_cur %>%
    inner_join(data_past, by = "County", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("County"))
  data_join$Difference <- (data_join$cases - data_join$cases_past)*10/14

#integrate datasets
  SwedenMap <- inner_join(geom,data_join, by = c("name"='County'))
  SwedenMap$RegionName = paste0(SwedenMap$name,", Sweden")
  SwedenMap$Country = "Sweden"
  SwedenMap$DateReport = as.character(SwedenMap$date)
  SwedenMap$pInf = SwedenMap$Difference/SwedenMap$Population
  SWEDEN_DATA = subset(SwedenMap,select=c("DateReport","RegionName","Country","pInf","geometry"))

  return(SWEDEN_DATA)
}
