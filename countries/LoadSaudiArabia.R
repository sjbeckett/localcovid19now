LoadSaudiArabia<-function(){
#Data sourced from Ministry of Health, Covid19 Command and Control Center CCC, The National Health Emergency Operation Center NHEOC; and assembled by National Health Command and Control NHCC, Covid19 Data and Informatics Committee. https://covid19.moh.gov.sa/

Casesdataset = st_read("https://services6.arcgis.com/bKYAIlQgwHslVRaK/arcgis/rest/services/VWPlacesCasesHostedView/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
#note need to convert dates from UNIX timestamp

#13 regions, 146 governates. Let's work with regions.

Regions = unique(Casesdataset$RegionName_EN)
DateReport=c()
CaseDifference=c()
RegionName=c()
for(aa in 1:length(Regions)){
	subsetdata = Casesdataset[which(Casesdataset$RegionName_EN==Regions[aa]),]
	DATES = as.Date(as.POSIXct(as.numeric(subsetdata$Reportdt)/1000, origin="1970-01-01")) #convert from UNIX timestamp
	LastDate = max(DATES)
	DateReport[aa] = as.character(LastDate)
	CaseDifference[aa] = (10/14)*sum(subsetdata$Confirmed[which(DATES>(LastDate-14))])
	RegionName[aa] = paste0(subsetdata$RegionName_AR[1],"/",subsetdata$RegionName_EN[1],", Saudi Arabia")
}
caseTable = data.frame(Regions,DateReport,CaseDifference,RegionName)

#pop
SApop = vroom("countries/data/SaudiArabiaPop.csv") #2017 census
SAmisc = vroom("countries/data/miscSaudiArabia.csv")
SAdf = inner_join(caseTable,SApop,by=c("Regions" = "Emirate"))%>%
  inner_join(SAmisc, by=c("Regions"="region_name_en"))

#geom
#geomSaudiArabia = st_read("https://services6.arcgis.com/bKYAIlQgwHslVRaK/arcgis/rest/services/CasesByRegion_ViewLayer/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
geomSaudiArabia = st_read("countries/data/geom/geomSaudiArabia.geojson")

#integrate datasets
MapSaudiArabia = inner_join(geomSaudiArabia,SAdf,by = c("micro_name" = "Regions"))

MapSaudiArabia$Country = MapSaudiArabia$country_name
MapSaudiArabia$pInf = MapSaudiArabia$CaseDifference/MapSaudiArabia$Population
SAUDI_ARABIA_DATA = subset(MapSaudiArabia,select=c("DateReport","RegionName","Country","pInf","geometry"))

return(SAUDI_ARABIA_DATA)
}