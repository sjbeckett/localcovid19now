LoadPeru<-function(){
#Data obtained from COVID-19 Data Hub https://covid19datahub.io
#sourced from https://github.com/jmcastagnetto/covid-19-peru-data

#Guidotti et al., (2020). COVID-19 Data Hub. Journal of Open Source Software, 5(51), 2376, https://doi.org/10.21105/joss.02376

x<-as.data.frame(covid19("Peru",level=2))

regions = unique(x$administrative_area_level_2)
DateReport=c()
CaseDifference=c()
pInf=c()
for(aa in 1:length(regions)){
	subsetdata = x[which(x$administrative_area_level_2==regions[aa]),]
	DateReport[aa] = as.character(max(subsetdata$date))
	#CaseDifference[aa] = (10/14)*diff(range(tail(subsetdata$confirmed,14)))
	## diff(range(tail(subsetdata$confirmed,14))) returns NA
	currentInd = nrow(subsetdata)
	CaseDifference[aa] = (10/14)*(subsetdata$confirmed[currentInd] - subsetdata$confirmed[currentInd-14])
	pInf[aa] = CaseDifference[aa]/subsetdata$population[1]
}

caseTable = data.frame(regions,DateReport,pInf)
caseTable$regionUpper = toupper(regions)
#geography
#geomPeru = st_read("https://github.com/juaneladio/peru-geojson/raw/master/peru_departamental_simple.geojson")
geomPeru = st_read("countries/data/geom/geomPeru.geojson")

#integrate datasets
MapPeru = inner_join(geomPeru,caseTable,by = c("NOMBDEP"="regionUpper"))

MapPeru$RegionName = paste0(MapPeru$regions,", Peru")
MapPeru$Country = "Peru"

PERU_DATA = subset(MapPeru,select=c("DateReport","RegionName","Country","pInf","geometry"))
return(PERU_DATA)
}
