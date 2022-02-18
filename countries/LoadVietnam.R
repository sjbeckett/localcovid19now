LoadVietnam <- function(){
#Official data on the COVID-19 epidemic in Vietnam collected by the Vietnam Ministry of Health & Ministry of Information and Communications. Operated by National Center for Technology for COVID-19 Prevention and Control. Developed by VN National Cyber Security Center. https://covid19.ncsc.gov.vn/dulieu

#provincedata <- read_json("https://covid19.ncsc.gov.vn/api/v3/covid/provinces")
#extract relevant province information
#ProvinceInfo=c()
#for(aa in 1:length(provincedata)){
	#ProvinceInfo$id[aa] = provincedata[[aa]]$id
	#ProvinceInfo$Name[aa] = provincedata[[aa]]$name
	#ProvinceInfo$Population[aa] = provincedata[[aa]]$population
#}
#ProvinceInfo=data.frame(ProvinceInfo)
#write.csv(ProvinceInfo,"countries/data/VietnamProvinceInfo.csv",row.names=FALSE)
ProvinceInfo = read.csv("countries/data/VietnamProvinceInfo.csv")

#Geometry
#geom = st_read("https://github.com/hausuresh/vietnam-geocode/raw/master/vietnam.geojson")
#geom$name[18] = ProvinceInfo$Name[57]
#geom$name[53] = ProvinceInfo$Name[2]
#geom$name[59] = ProvinceInfo$Name[40]
#st_write(geom,"geomVietnam.geojson")
geomVietnam = st_read("countries/data/geom/geomVietnam.geojson")
#need to do some work to get the naming to match up
ProvinceInfo$NameUse = geomVietnam$micro_name[ProvinceInfo$code]

#look at case data
data <- read_json("https://covid19.ncsc.gov.vn/api/v3/covid/provinces?filter_type=case_by_time")
provinceID = names(data)

CaseDiff = c()
DateReport = c()
code = c()

#most recent data has same cumulative entries as the prior day. Suggest not using the most recent day in calculations.
for(aa in 1:length(provinceID)){
	MostRecent = tail(data[[aa]],15)
	currentCases = MostRecent[14]
	pastCases = MostRecent[1]
	CaseDiff[aa] = (10/14)*(currentCases[[1]] - pastCases[[1]])
	DateReport[aa] = tail(names(data[[2]]),2)[1]
	code[aa] = as.numeric(provinceID[aa])
}
#format date
DateReport = as.character(as.Date(DateReport,format="%d/%m/%Y"))
dataTable <- data.frame(DateReport = DateReport, Code = code, Difference = CaseDiff)

datadf = inner_join(dataTable,ProvinceInfo, by = c("Code" = "id"))


VietnamMap <-inner_join(geomVietnam, datadf, by = c("micro_name" = "NameUse"))
VietnamMap$Country = "Vietnam"
VietnamMap$RegionName = paste(VietnamMap$name, VietnamMap$country_name, sep=", ")
VietnamMap$pInf = as.numeric(VietnamMap$Difference)/as.numeric(VietnamMap$Population)
Vietnam_DATA = subset(VietnamMap,select=c("DateReport","RegionName","Country","pInf","geometry"))
return(Vietnam_DATA)
}