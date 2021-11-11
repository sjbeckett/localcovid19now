LoadSmallCountries<-function(){
#Aggregated by the COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University
#https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
#Dong, E., Du, H., & Gardner, L. (2020). An interactive web-based dashboard to track COVID-19 in real time. The Lancet infectious diseases, 20(5), 533-534.

CountryList = c("Singapore","Brunei","Djibouti","Qatar","Marshall Islands","Saint Kitts and Nevis","Timor-Leste","Maldives","Grenada","Saint Vincent and the Grenadines","Saint Lucia","Barbados","Antigua and Barbuda","Seychelles","Palau","Micronesia","Dominica","Bahrain","Kiribati","Sao Tome and Principe","Comoros","Mauritius","Samoa","Trinidad and Tobago","Lebanon","Jamaica","Gambia","Vanuatu","Bahamas","Eswatini","Kuwait","Fiji","El Salvador","Belize","Cabo Verde","West Bank and Gaza")

ProvinceList = c("Faroe Islands","Falkland Islands (Malvinas)","Greenland","New Caledonia","Turks and Caicos Islands","Anguilla","British Virgin Islands","Gibraltar","Bermuda","Sint Maarten","Aruba","Curacao","Cook Islands")

#load cases data
data <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')

# get updated date:
date <- names(data)[length(names(data))]
date <- strsplit(date,'X')[[1]][2]
date <- as.Date(date, format = "%m.%d.%y")

DateReport=c()
CaseDifference=c()
for(aa in 1:length(CountryList)){
	subsetdata = data[which(data$Country.Region==CountryList[aa]),]
	DateReport[aa] = as.character(date)
	CaseDifference[aa] = (10/14)*diff(range(subsetdata[(length(subsetdata)-14):(length(subsetdata))]))
}
LL = aa

for(aa in 1:length(ProvinceList)){
	subsetdata = data[which(data$Province.State==ProvinceList[aa]),]
	DateReport[LL+aa] = as.character(date)
	CaseDifference[LL+aa] = (10/14)*diff(range(subsetdata[(length(subsetdata)-14):(length(subsetdata))]))
}

caseTable = data.frame(CountryList = c(CountryList,ProvinceList),DateReport,CaseDifference)
caseTable$CountryList[which(caseTable$CountryList=="Sao Tome and Principe")] = "São Tomé and Príncipe"
caseTable$CountryList[which(caseTable$CountryList=="Curacao")] = "Curaçao"
caseTable$CountryList[which(caseTable$CountryList=="Falkland Islands (Malvinas)")] = "Falkland Islands"

#Geography
geomGL = st_read("countries/data/geom/WB_countries_Admin0_lowres.geojson") #from world bank https://datacatalog.worldbank.org/dataset/world-bank-official-boundaries
geomGL$NAME_EN[which(geomGL$NAME_EN=="Federated States of Micronesia")] = "Micronesia"
geomGL$NAME_EN[which(geomGL$NAME_EN=="The Bahamas")] = "Bahamas"
geomGL$NAME_EN[which(geomGL$NAME_EN=="The Gambia")] = "Gambia"
geomGL$NAME_EN[which(geomGL$NAME_EN=="eSwatini")] = "Eswatini"
geomGL$NAME_EN[which(geomGL$NAME_EN=="East Timor")] = "Timor-Leste"
geomGL$NAME_EN[which(geomGL$NAME_EN=="Cape Verde")] = "Cabo Verde"
geomGL$NAME_EN[which(geomGL$NAME_EN=="Palestine")] = "West Bank and Gaza"




#integrate datasets
MapGL = inner_join(geomGL,caseTable,by=c("NAME_EN"="CountryList"))
MapGL$RegionName = MapGL$NAME_EN
MapGL$Country = MapGL$NAME_EN
MapGL$DateReport = as.character(date) 
MapGL$pInf = MapGL$CaseDifference/MapGL$POP_EST

COUNTRIES = subset(MapGL,select=c("DateReport","RegionName","Country","pInf","geometry"))
return(COUNTRIES)
}