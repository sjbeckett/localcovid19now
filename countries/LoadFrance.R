LoadFrance <- function() {
#Santé publique France COVID-19 data for France :  https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/  
#Note this resource also contains data for overseas departments of France, and for Saint Barthélemy, Saint Martin, and Saint Pierre and Miquelon.
  
  #data <- read.csv('https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675',sep = ';', stringsAsFactors = FALSE)
  data <- vroom::vroom('https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675')  %>%
    filter(cl_age90 == 0) %>% 
    select(code = dep, date = jour, cases = P) %>%
    mutate(date = as.Date(date)) %>% 
    filter(!is.na(cases)) 
  #geom <<- st_read('https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-avec-outre-mer.geojson')
  geom <- st_read("countries/data/geom/geomFrance.geojson") # does not contain Saint Barthélemy, Saint Martin, and Saint Pierre and Miquelon.
  pop <- read.csv("countries/data/france_pop.csv", stringsAsFactors = FALSE,encoding="UTF-8") %>% select(code = Code, name = Department, pop = Population)
  
  depList <- unique(data$code) # get the list of all department codes
  
  # sort out and calculate the number of cases during two recent weeks
  # depList[code] = corresponding code of a department
  sortFunc <- function(code){
    deptCode <- depList[code] 
    department <- data %>% filter(code == deptCode) %>% distinct(date, .keep_all = TRUE) 
    latestDate <- department$date[length(department$date)]
    pastDate <- latestDate - 14
    difference <- (sum(department[1:which(department$date == latestDate),'cases']) - sum(department[1:which(department$date == pastDate),'cases']))/14*10
    vec <- data.frame(code = depList[code], date = latestDate, n = difference)
    return(vec)
  }
  
  # get the data table that includes department codes, last updated date, difference between 14 days
  frenchTable <- data.frame()
  for (i in 1:length(depList)){
    vec <- sortFunc(i)
    frenchTable <- rbind(frenchTable,vec)
  }
  frenchdf <- inner_join(frenchTable, pop, by = 'code')
  FranceMap <- inner_join(geom,frenchdf, by = c('micro_code'='code'))
  FranceMap$RegionName = paste(FranceMap$micro_name,FranceMap$country_name, sep = ", ")
  FranceMap$Country = FranceMap$country_name
  FranceMap$DateReport = as.character(FranceMap$date) 
  FranceMap$pInf = FranceMap$n/FranceMap$pop
  
  FRANCE_DATA = subset(FranceMap,select=c("DateReport","RegionName","Country","pInf","geometry"))
  return(FRANCE_DATA)
}
