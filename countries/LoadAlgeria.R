LoadAlgeria<-function(){
#Algeria Coronavirus Tracker API https://corona-dz.live/ https://github.com/Amine27/covid-19-dz

ALG = fromJSON("https://api.corona-dz.live/province/all")


#geography
#https://github.com/Amine27/covid-19-dz/blob/master/static/map/algeria.json
#geomAlgeria = st_read("https://github.com/Amine27/covid-19-dz/raw/master/static/map/algeria.json")
geomAlgeria = st_read("countries/data/geom/geomAlgeria.geojson")

name2save1 = geomAlgeria$micro_name[22] #"Sidi Bel Abbès"
name2save2 = geomAlgeria$micro_name[44] #"Aïn Defla"
name2save3 = geomAlgeria$micro_name[46] #"Aïn Témouchent"

#regions
provinces = ALG$name
provinces[provinces=="Ain Defla"] = name2save2
provinces[provinces=="Ain Temouchent"] = name2save3
provinces[provinces=="Algiers"] = "Alger"
provinces[provinces=="Oum El Bouaghi"] = "Oum el Bouaghi"
provinces[provinces=="Sidi Bel Abbes"] = name2save1
provinces[provinces=="Tamanrasset"] = "Tamanghasset"


DateReport=c()
CaseDifference=c()
for(aa in 1:length(provinces)){
	subsetdata = ALG$data[[aa]]
	DateReport[aa] = as.character(max(as.Date(subsetdata$date)))
	CaseDifference[aa] = sum(subsetdata$newConfirmed[as.Date(subsetdata$date)> max(as.Date(subsetdata$date)) - 14])*(10/14)	
}

caseTable = data.frame(provinces,DateReport,CaseDifference)

#population
pop = vroom("countries/data/algeria_pop.csv") #2008 census

Algeriadf = inner_join(caseTable,pop,by=c("provinces"="Name"))



AlgeriaMap = inner_join(geomAlgeria,Algeriadf, by = c('micro_name' = 'provinces'))
AlgeriaMap$pInf = AlgeriaMap$CaseDifference/AlgeriaMap$population
AlgeriaMap$RegionName = paste(AlgeriaMap$micro_name,AlgeriaMap$country_name, sep=", ")
AlgeriaMap$Country = AlgeriaMap$country_name
ALGERIA_DATA = subset(AlgeriaMap,select=c("DateReport","RegionName","Country","pInf","geometry"))

return(ALGERIA_DATA)
}
