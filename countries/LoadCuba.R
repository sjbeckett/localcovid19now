LoadCuba<-function(){
#Data aggregated from local health resources by Covid19CubaData http://covid19cubadata.github.io/

#Old dataset -> July 4 2021
#cuba_data = read.csv("https://covid19cubadata.github.io/data/covid19-casos.csv", encoding = 'UTF-8')
#cuba_data <- cuba_data %>%
#    dplyr::count(fecha_confirmacion, provincia) %>%
#    dplyr::select(date = fecha_confirmacion, region_level_1 = provincia, cases_new = n) %>%
#    dplyr::filter(!is.na(region_level_1)) %>%
#    dplyr::mutate(
#      cases_new = as.numeric(cases_new),
#      date = lubridate::as_date(lubridate::ymd(date))
#    )

#PRO = unique(cuba_data$region_level_1)
#LATEST = tail(cuba_data$date,1)
#DiffCases=c()

#for(aa in 1:(length(PRO)-1)){
#	subsetdata = dplyr::filter(cuba_data,region_level_1==PRO[aa])
#	IND = which(subsetdata$date>(LATEST-14))
#	DiffCases[aa] = sum(subsetdata$cases_new[IND])*10/length(IND)
#}

#data from August 4 2021
CUBADATA2 = fromJSON("https://github.com/covid19cubadata/covid19cubadata.github.io/raw/master/data/covid19-cuba-2.json")
dataset = CUBADATA2$casos$dias
DATES = names(dataset)
TODAY = c()
CASES = c()
cuba=c()
#PRO = unique(dataset[[DATES[1]]]$diagnosticados$provincia_detección) #provinces

#geomCuba = st_read("https://raw.githubusercontent.com/covid19cubadata/covid19cubadata.github.io/master/data/provincias.geojson")
geomCuba<-st_read("countries/data/geom/geomCuba.geojson")
geomCuba$pop = c(2154454,679314,400768,462114,525729,1027683,830645,1053837,84263,768311,424750,783708,506369,585452,487339,371198) #2015 census in Cuba

PRO  = geomCuba$micro_name

for(aa in 1:length(DATES)){
	subsetdata = dataset[[DATES[aa]]]
	names(subsetdata$diagnosticados) = c("id","pais","municipio","provincia","codemunicipo","codeprovince","contagio","contacto_focal") #diacritics might affect loading. rename columns to avoid this.
	TODAY = as.character(as.Date(subsetdata$fecha))
	for(bb in 1:length(PRO)){
		CASES[bb] = sum(subsetdata$diagnosticados$provincia==PRO[bb])
	}	
	cuba = rbind(cuba,c(TODAY,CASES))
}
cuba_data = data.frame(cuba)
names(cuba_data) = c("date",PRO)

#print(cuba_data)

LATEST = tail(cuba_data$date,1)
DiffCases=c()
IND = which(cuba_data$date>(as.Date(LATEST)-14))
CASES = matrix(as.numeric(as.matrix(cuba_data[,-1])),nrow =nrow(cuba_data[,-1]))
DiffCases = colSums(CASES[IND,])*10/14

cubaTable=c()
cubaTable$Pro = PRO #PRO[1:(length(PRO)-1)]
cubaTable$DiffCases = DiffCases
cubaTable$date = LATEST
cubaTable = as.data.frame(cubaTable)

#print(cubaTable)


MapCuba = inner_join(geomCuba,cubaTable,by=c("micro_name"="Pro"))

MapCuba$RegionName = paste(MapCuba$micro_name, MapCuba$country_name, sep=", ")
MapCuba$Country = MapCuba$country_name
MapCuba$DateReport = as.character(MapCuba$date) 
MapCuba$pInf = MapCuba$DiffCases/MapCuba$pop
CUBA_DATA = subset(MapCuba,select=c("DateReport","RegionName","Country","pInf","geometry"))
return(CUBA_DATA)
}

