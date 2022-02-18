LoadOman = function(){
#Ministry of Health for Oman, collated by Safeture for the Humanitarian Data Exchange: https://data.humdata.org/dataset/oman-coronavirus-covid-19-subnational-cases

CaseData = read.csv("https://www.dropbox.com/s/ylop7xswywi147c/cases_oman.csv?dl=1")
Governorates = sort(unique(CaseData$name))
CaseDiff=c()
DateReport=c()

for(aa in 1:length(Governorates)){
	subsetdata = CaseData[which(CaseData$name == Governorates[aa]),]
	CaseDiff[aa] = (10/14)*sum(tail(subsetdata$cases,14))
	DateReport[aa] = tail(subsetdata$date,1)
}
omandt = data.frame(DateReport,Name = Governorates,Difference = CaseDiff)

#population
pop=c()
pop$Name = c("Ad Dakhiliyah", "Al Buraymi", "Al Wusta", "Ad Dhahirah", "Al Batinah South", "Ash Sharqiyah South", "Muscat", "Musandam", "Al Batinah North", "Ash Sharqiyah North", "Dhofar")
pop$Population = c(478501,121802,52344,213043,465550,315445,1302440,49062,784681,271822,416458) #2020 census - https://www.citypopulation.de/en/oman/cities/
pop = data.frame(pop)

omandf = inner_join(omandt,pop,by=c("Name"="Name"))

#geometry
#https://data.humdata.org/dataset/cod-ab-omn?
geomOman = st_read("countries/data/geom/geomOman.geojson")
geomOman$ADM1_EN[5] =  "Al Dhahirah"
geomOman$ADM1_EN[1] = "Ad Dakhiliyah"

OmanMap <- inner_join(geomOman, omandf, by = c("ADM1_EN" = "Name"))
OmanMap$country_name = "Oman"
OmanMap$RegionName = paste(OmanMap$ADM1_EN, OmanMap$country_name, sep=", ")
OmanMap$pInf = as.numeric(OmanMap$Difference)/as.numeric(OmanMap$Population)
Oman_DATA = subset(OmanMap,select=c("DateReport","RegionName","country_name","pInf","geometry"))

return(Oman_DATA)
}

