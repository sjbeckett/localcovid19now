LoadEurope <- function() {
#Data aggregated from local health resources in the WHO European Region COVID19 Subnational Explorer https://experience.arcgis.com/experience/3a056fc8839d47969ef59949e9984a71

geomEUROPE = st_read("countries/data/geom/EUROPE.geojson")

#most uptodate data
#EUWHO = read.csv("https://arcgis.com/sharing/rest/content/items/54d73d4fd4d94a0c8a9651bc4cd59be0/data",encoding="UTF-8")
EUWHO = vroom::vroom("https://arcgis.com/sharing/rest/content/items/54d73d4fd4d94a0c8a9651bc4cd59be0/data",col_types=c(DateRpt="c"))


EUWHO$pInf = EUWHO$Incidence14day/100000*(10/14) #incidence is cases in 14 days per 100,000 people. Convert to prop of pop. in 10 days.
#Join using UID
EuroMap = inner_join(geomEUROPE,EUWHO,by = c("UID" = "UID"))

##Remove countries
#Turkmenistan - no testing performed. Hence no cases found.
EuroMap = EuroMap[-which(EuroMap$ADM0_NAME=="Turkmenistan"),]
#Countries which we have higher resolution maps
CountriesCovered = c("Italy","Switzerland","Ireland","United Kingdom","Austria","France","Czech Republic","Spain","Denmark","Sweden","Netherlands","Germany","Norway","Belgium","Liechtenstein")
RM_REGIONS = c()
for(aa in 1:length(CountriesCovered)){
	INDXs = which(EuroMap$ADM0_NAME==CountriesCovered[aa])
	if(CountriesCovered[aa]=="United Kingdom"){ #keep UK crown dependencies.
			INDXs = INDXs[-which(EuroMap$Region[INDXs]=="Isle of Man")]
			INDXs = INDXs[-which(EuroMap$Region[INDXs]=="Guernsey")]
			INDXs = INDXs[-which(EuroMap$Region[INDXs]=="Jersey")]
	}
	RM_REGIONS = c(RM_REGIONS, INDXs )
}
EuroMap = EuroMap[-RM_REGIONS,]	
EuroMap$RegionName = paste0(EuroMap$RegionName,", ",EuroMap$ADM0_NAME)
EuroMap$Country  = EuroMap$ADM0_NAME

EUROPE_DATA = subset(EuroMap, select = c("DateRpt","RegionName","Country","pInf","geometry"))
colnames(EUROPE_DATA)[1] = "DateReport"

return(EUROPE_DATA)
}