LoadNewZealand<-function(){
#COVID-19 Data Repository by the Institute of Environmental Science and Research: https://github.com/ESR-NZ/NZ_COVID19_Data/
#Institute for Environmental Science and Research, New Zealand COVID19 Dashboard. https://nzcoviddashboard.esr.cri.nz/ (2020).
#Jefferies, Sarah, et al. "COVID-19 in New Zealand and the impact of the national response: a descriptive epidemiological study." The Lancet Public Health 5.11 (2020): e612-e623.

# New Zealand's dashboard and system is currently down due to a system upgrade, hence why this isn't finding anything.
  
#load case data
#need to try 2 days if it doesn't work!
flag=0
aa=0
while(flag==0){
	DATE = Sys.Date()-aa
	formDATE = format(DATE, "%Y-%m-%d")
	STRING = paste0("https://github.com/ESR-NZ/NZ_COVID19_Data/raw/master/overview_case/",formDATE,".csv")
	NZ<- try(vroom(STRING))
	if (is.null(dim(NZ)) == FALSE){
		flag=1
	}else{
		aa=aa+1
	}
	if(aa>5){
		warning("no recent data")
		flag=2
	}
}

Regions = unique(NZ$DHBName)
DateReport = c()
DateRecent = c()
CaseDifference = c()

for(aa in 1:length(Regions)){
	subsetdata = NZ[which(NZ$DHBName == Regions[aa]),]
	DateRecent[aa] = max(subsetdata$ReportDate)
	eligibleC = subsetdata$Confirmed[which(as_date(subsetdata$ReportDate)>as_date(DateRecent[aa])-14)]
	CaseDifference[aa] = (10/14)*sum(eligibleC)
	DateReport[aa] = as.character(as_date(DateRecent[aa]))
}

CaseTable = data.frame(Regions,DateReport,CaseDifference)

#population for 2020.
#http://nzdotstat.stats.govt.nz/wbos/Index.aspx?DataSetCode=TABLECODE7509
Pop=c()
Pop$Place = c("Northland","Waitemata","Auckland","Counties Manukau","Waikato","Lakes","Bay of Plenty","Tairawhiti","Taranaki","Hawke's Bay","Whanganui","MidCentral","Hutt Valley","Capital and Coast","Wairarapa","Nelson Marlborough","West Coast","Canterbury","South Canterbury","Southern")
Pop$population = c(172100,575100,472800,522500,393000,106800,226100,48100,116800,163600,64000,173600,146700,302500,44000,147400,32800,525700,58800,316900)
Pop = as.data.frame(Pop)

NZdf = inner_join(CaseTable,Pop,by=c("Regions"="Place"))


#geography
#District health board regions https://datafinder.stats.govt.nz/layer/87883-district-health-board-2015/
#https://github.com/leon-sleepinglion/NZ-COVID-19-Visualization
#geomNZ = st_read("https://github.com/leon-sleepinglion/NZ-COVID-19-Visualization/raw/master/dhb.geojson")
geomNZ = st_read("countries/data/geom/geomNZ.geojson")
Name2Move = geomNZ$micro_name[8]
geomNZ$micro_name[8] = "Tairawhiti"

#integrate datasets
MapNZ = inner_join(geomNZ,NZdf, by = c("micro_name" = "Regions"))
MapNZ$micro_name[8] = Name2Move
MapNZ$RegionName = paste(MapNZ$micro_name,MapNZ$country_name, sep=", ")
MapNZ$Country = MapNZ$country_name
MapNZ$pInf = MapNZ$CaseDifference/MapNZ$population

NEWZEALAND_DATA = subset(MapNZ,select=c("DateReport","geoid","RegionName","Country","pInf","geometry"))
return(NEWZEALAND_DATA)
}
