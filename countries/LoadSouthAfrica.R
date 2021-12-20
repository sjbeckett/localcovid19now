LoadSouthAfrica<-function(){
#Data Science for Social Impact Research Group @ University of Pretoria, Coronavirus COVID-19 (2019-nCoV) Data Repository for South Africa. Available on: https://github.com/dsfsi/covid19za

#geometry, see: https://dataportal-mdb-sa.opendata.arcgis.com/datasets/23e1b3458c704f65bc764168ae8557b8_0/data?geometry=-17.204%2C-35.168%2C66.424%2C-21.692&selectedAttribute=PROVINCE
#geomSA$PROVINCE[geomSA$PROVINCE=="GT"]="GP"
#geomSA$PROVINCE[geomSA$PROVINCE=="LIM"]="LP"
geomSA=st_read("countries/data/geom/geomSouthAfrica.geojson")
PRO = unique(geomSA$PROVINCE)

#covid case data, see: https://github.com/dsfsi/covid19za
COVID_data_SA<- vroom("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv")

DiffCases = c()
Date=c()
for(aa in 1:length(PRO)){
	THIS = select(COVID_data_SA,PRO[aa])
	DiffCases[aa] = diff(range(tail(THIS,14)))*10/14
	Date[aa] = as.character(tail(as.Date(COVID_data_SA$date,format="%d-%m-%Y"),1))
}
geomSA$DiffCases = DiffCases
geomSA$date = Date

#population data, see: https://github.com/dsfsi/covid19za/blob/master/data/district_data/za_province_pop.csv
#SA_province_pop <-read.csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/district_data/za_province_pop.csv",header=FALSE)
SA_province_pop <-vroom("countries/data/SA_province_pop.csv", col_names=FALSE)


PROVINCECode = c("GP","KZN","WC","EC","LP","MP","NW","FS","NC")
SA_province_pop=cbind(SA_province_pop,PROVINCECode)
names(SA_province_pop) = c("Name","Population","ProCode")

#Link together
SA_Map = inner_join(geomSA,SA_province_pop,by=c("micro_code" = "ProCode"))
SA_Map$RegionName = paste(SA_Map$Name, SA_Map$country_name, sep = ", ")
SA_Map$Country = SA_Map$country_name
SA_Map$DateReport = SA_Map$date
SA_Map$pInf = SA_Map$DiffCases/SA_Map$Population
SA_DATA = subset(SA_Map,select=c("DateReport","RegionName","Country","pInf","geometry"))
return(SA_DATA)
}