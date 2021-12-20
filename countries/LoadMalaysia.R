LoadMalaysia <- function(){
#Official data on the COVID-19 epidemic in Malaysia. Powered by CPRC, CPRC Hospital System, MKAK, and MySejahtera.  https://github.com/MoH-Malaysia/covid19-public

casesbystate <- vroom("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_state.csv") #new cases by state by time
casesbystate$date = as_date(casesbystate$date)
states = unique(casesbystate$state)


#population
#pop  = read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/static/population.csv")
pop = vroom("countries/data/MalaysiaPop.csv") %>% select(state,pop)

DateReport=c()
pInf=c()
for(aa in 1:length(states)){
	subset = casesbystate[casesbystate$state==states[aa],]
	DateReport[aa] = as.character(max(subset$date))
	CaseDifference = sum(subset$cases_new[which(subset$date>max(subset$date)-14)])/14*10
	pInf[aa] = CaseDifference/pop$pop[pop$state==states[aa]]
}
dataTable = data.frame(state = states,DateReport,pInf)

#geometry
#geomMalaysia = st_read("https://raw.githubusercontent.com/jnewbery/MapMalaysia/master/public/data/states.geojson")
#edit names to match with case data
#geomMalaysia$Name[1] = "W.P. Kuala Lumpur"
#geomMalaysia$Name[2] = "W.P. Labuan"
#geomMalaysia$Name[3] = "W.P. Putrajaya"
#geomMalaysia$Name[10] = "Pulau Pinang"
geomMalaysia = st_read("countries/data/geom/geomMalaysia.geojson")


#integrate datasets
MalaysiaMap <- inner_join(geomMalaysia,dataTable, by = c("micro_name" = "state"))
MalaysiaMap$Country = MalaysiaMap$country_name
MalaysiaMap$RegionName = paste0(MalaysiaMap$Name,MalaysiaMap$Country, sep=", ")

MALAYSIA = subset(MalaysiaMap,select=c("DateReport","RegionName","Country","pInf","geometry"))

return(MALAYSIA)
}