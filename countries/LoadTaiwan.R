LoadTaiwan<-function(){
#sourced from Taiwan Centers for Disease Control https://data.cdc.gov.tw/en/dataset/aagsdctable-day-19cov

#x<- read.csv("https://od.cdc.gov.tw/eic/Day_Confirmation_Age_County_Gender_19CoV.csv",encoding = "UTF-8")
x<- vroom::vroom("https://od.cdc.gov.tw/eic/Day_Confirmation_Age_County_Gender_19CoV.csv")
names(x) = c("Disease Name","Date_Confirmation","County_living","Town_living","Sex","Imported","Age_Group","Number_of_confirmed_cases")

regions = unique(x$County_living)
DateReport=c()
CaseDifference=c()
for(aa in 1:length(regions)){
	subsetdata = x[which(x$County_living==regions[aa]),]
	Lastdate = max(as.Date(subsetdata$Date_Confirmation))
	DateReport[aa] = as.character(Lastdate)
	caseindex = which(as.Date(subsetdata$Date_Confirmation)>(Lastdate-14))
	CaseDifference[aa] = (10/14)*sum(subsetdata$Number_of_confirmed_cases[caseindex])
}
caseTable = data.frame(regions,DateReport,CaseDifference)

#population
Pop = read.csv("countries/data/TaiwanPop.csv",encoding="UTF-8")
TWdf = inner_join(caseTable,Pop,by=c("regions"="Chinese.name"))

#geometry
#geomTaiwan<- st_read("https://github.com/g0v/twgeojson/raw/master/json/twCounty2010.topo.json")
#geomTaiwan$COUNTYNAME[which(geomTaiwan$COUNTYNAME=="台北縣")] = "新北市"
##merge "台南市" "台南縣" as "台南市"
#Index = which(geomTaiwan$COUNTYNAME=="台南市")
#HMM<- st_union(geomTaiwan[c(Index,which(geomTaiwan$COUNTYNAME=="台南縣")),])%>% st_cast("MULTIPOLYGON")
#geomTaiwan$COUNTYNAME[Index] = "台南市"
#geomTaiwan$geometry[Index] = HMM
##merge "台中市" "台中縣" as  "台中市"
#Index = which(geomTaiwan$COUNTYNAME=="台中市")
#HMM<- st_union(geomTaiwan[c(which(geomTaiwan$COUNTYNAME=="台中市"),which(geomTaiwan$COUNTYNAME=="台中縣")),])%>% st_cast("MULTIPOLYGON")
#geomTaiwan$COUNTYNAME[Index] = "台中市"
#geomTaiwan$geometry[Index] = HMM
#geomTaiwan$COUNTYNAME[which(geomTaiwan$COUNTYNAME=="桃園縣")] = "桃園市"
#geomTaiwan$COUNTYNAME[which(geomTaiwan$COUNTYNAME=="高雄縣")] = "高雄市"
#geomTaiwan$COUNTYNAME[which(geomTaiwan$COUNTYNAME=="台南縣")] = "台南市" #Tainan City
#geomTaiwan$COUNTYNAME[which(geomTaiwan$COUNTYNAME=="台中縣")] = "台中市" #Taichung City

geomTaiwan = st_read("countries/data/geom/geomTaiwan.geojson")


#integrate datasets
MapTaiwan = inner_join(geomTaiwan,TWdf,by = c("micro_name"="regions"))

MapTaiwan$RegionName = paste(paste(MapTaiwan$micro_name, MapTaiwan$English.name, sep="/"), MapTaiwan$country_name, sep = ", ")
MapTaiwan$Country = MapTaiwan$country_name
MapTaiwan$pInf = MapTaiwan$CaseDifference/MapTaiwan$Population.2020

TAIWAN_DATA = subset(MapTaiwan,select=c("DateReport","RegionName","Country","pInf","geometry"))
return(TAIWAN_DATA)
}


