LoadSouthKorea = function(){
#collated by https://github.com/staedi/nCOV-summary/ from the South Korea CDC: http://ncov.mohw.go.kr/

data = read.csv("https://raw.githubusercontent.com/staedi/nCOV-summary/master/time_series_covid19_infections.csv")
SKOR = data[which(data$adm0_a3=="KOR"),]

Provinces= unique(SKOR$Province.State)
Population=c()
CaseDiff=c()
DateReport=c()
for(aa in 1:length(Provinces)){
	subsetdata = SKOR[which(SKOR$Province.State == Provinces[aa]),]
	Population[aa] = subsetdata$Population[1]
	CaseDiff[aa] = (10/14)*diff(range(tail(subsetdata$confirmed,14)))
	DateReport[aa] = as.character(as.Date(tail(subsetdata$Date,1),format = "%m/%d/%y"))
}

sk_df = data.frame(DateReport,Provinces,CaseDiff,Population)

#geometry
#https://maps.princeton.edu/catalog/stanford-dk009rq9138
#geomSouthKorea = stwrite(SK2,"geomSouthKorea.geojson")
geomSouthKorea = st_read("countries/data/geom/geomSouthKorea.geojson")
#get data for alternative label
alt = read.csv("countries/data/miscSouthKorea.csv",encoding="UTF-8")

SouthKoreaMap <-inner_join(geomSouthKorea, sk_df, by = c("micro_name" = "Provinces"))
SouthKoreaMap <-inner_join(SouthKoreaMap , alt, by = c("micro_name" = "name_1"))
Regions = paste0(SouthKoreaMap$nl_name_1,"/",SouthKoreaMap$micro_name)

SouthKoreaMap$Country = "South Korea"
SouthKoreaMap$RegionName = paste(Regions, SouthKoreaMap$country_name, sep=", ")
SouthKoreaMap$pInf = as.numeric(SouthKoreaMap$CaseDiff)/as.numeric(SouthKoreaMap$Population)
SouthKorea_DATA = subset(SouthKoreaMap,select=c("DateReport","geoid","RegionName","Country","pInf","geometry"))
return(SouthKorea_DATA)
}