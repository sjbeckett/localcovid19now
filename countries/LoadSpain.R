LoadSpain <- function(){
#COVID-19 data from España Ministerio de Sanidad and Instituto de Salud Carlos III: https://cnecovid.isciii.es/covid19/ 

#county geojson data
#geomSPAIN <- st_read('https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/spain-provinces.geojson')
geomSPAIN <- st_read("countries/data/geom/geomSpain.geojson")

#Main COVID-19 hub page: https://cnecovid.isciii.es/covid19/#distribuci%C3%B3n-geogr%C3%A1fica
SPAIN<- read.csv("https://cnecovid.isciii.es/covid19/resources/casos_tecnica_provincia.csv",na.strings=FALSE) 

#code link file
Spaincode = read.csv("countries/data/spain_codenames.csv",encoding="UTF-8",na.strings=FALSE)
#Population data comes from  Instituto Nacional de Estadística: https://www.ine.es/jaxiT3/Datos.htm?t=2852#!tabs-tabla

DataJoin = c()
Counties <- unique(SPAIN$provincia_iso)
DataJoin$ProvinceName = Counties
for(aa in 1:length(Counties)){
	Subset = SPAIN[SPAIN$provincia_iso==Counties[aa],]
	Dates = as.Date(Subset$fecha)
	LL = length(Dates)
	ConfirmedCovidCases = cumsum(Subset$num_casos)
	#CaseDiff = 10*( Subset$ConfirmedCovidCases[LL] - Subset$ConfirmedCovidCases[LL - 14])/ 14
	CaseDiff  = (ConfirmedCovidCases[LL] - ConfirmedCovidCases[LL - 14])/14*10
	#Make sure difference in cases is positive. If not set to NA.
	if(CaseDiff<0){
		CaseDiff = NA
	}
	DataJoin$LatestTime[aa] = as.character(Dates[LL])
	DataJoin$CaseDiff[aa] = CaseDiff
}


#integrate datasets
Spaindata = as.data.frame(DataJoin)
Spaincode = as.data.frame(Spaincode)
Spaindf = inner_join(Spaindata,Spaincode,by=c("ProvinceName"="code"))
SpainMap <- inner_join(geomSPAIN,Spaindf, by = "micro_name")

SpainMap$RegionName = paste(SpainMap$micro_name, SpainMap$country_name, sep = ", ")
SpainMap$Country = SpainMap$country_name
SpainMap$DateReport = as.character(SpainMap$LatestTime) 
SpainMap$pInf = SpainMap$CaseDiff/SpainMap$population2019
SPAIN_DATA = subset(SpainMap,select=c("DateReport","RegionName","Country","pInf","geometry"))

return(SPAIN_DATA)
}