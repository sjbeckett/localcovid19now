LoadCanada <- function(){
# Data aggregated by the COVID-19 Canada Open Data Working Group https://github.com/ccodwg/Covid19Canada
# Berry I, Soucy J-PR, Tuite A, Fisman D. Open access epidemiologic data and an interactive dashboard to monitor the COVID-19 outbreak in Canada. CMAJ. 2020 Apr 14;192(15):E420. doi:  https://doi.org/10.1503/cmaj.75262.


CANADADATA<- vroom::vroom("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_hr/cases_timeseries_hr.csv")

#geomCAN= st_read("OpenContent_CoVID19Boundary_2020/OpenContent_COVID19Boundary_2020/data/covidHR.shp")
#geomCAN = st_transform(geomCAN, "+proj=longlat +datum=WGS84")
geomCAN = st_read("countries/data/geom/CanadaRegionalHealthBoundaries.geojson")

CAN_LINK<- read.csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/other/hr_map.csv",encoding="UTF-8")#contains pop and health region ID codes

#shrink this data file
DATES = as.Date(CANADADATA$date_report,format="%d-%m-%Y")
DatIND = which(DATES<"2020-10-01")
CANDATASMALLER = CANADADATA[-DatIND,]

#Assemble the data required to calculate risk scores and match health region ID's
CANADARISK = c()
count = 1
CPro = unique(CAN_LINK$province)
for( aa in 1:length(CPro) ){
	subsetCANL = CAN_LINK[CAN_LINK$province==CPro[aa],]
	subsetCAND = CANDATASMALLER[CANDATASMALLER$province==CPro[aa],]
	CHR = unique(subsetCANL$health_region)
	for (bb in 1:length(CHR)) {
		subset2CAND = subsetCAND[subsetCAND$health_region==CHR[bb],]
		subset2CANL = subsetCANL[subsetCANL$health_region==CHR[bb],]
		CANDATES = as.Date(subset2CAND$date_report,format="%d-%m-%Y")
		
		MD = max(CANDATES)
		MC = subset2CAND$cumulative_cases[which(CANDATES==MD)]
		LD = MD-14
		LC = subset2CAND$cumulative_cases[which(CANDATES==LD)]
		
		CANADARISK$province[count] = CPro[aa]
		CANADARISK$health_region[count] = CHR[bb]
		CANADARISK$HR_UID[count] = as.character(subset2CANL$HR_UID)
		CANADARISK$pop[count] = subset2CANL$pop
		CANADARISK$province_full[count] = subset2CANL$province_full
		CANADARISK$DateReport[count] = as.character(MD)
		CANADARISK$CaseDiff[count] = (MC - LC)/14*10
		CANADARISK$pInf[count] = CANADARISK$CaseDiff[count]/CANADARISK$pop[count]
		count = count + 1
	}
}

CANADARISK = as.data.frame(CANADARISK)
#Edit region codes for SK
geomCAN$HR_UID[95:96] = c("475","474")
#Merge polygons in SK
#Merge Central West and East
HMM<- st_union(geomCAN[c(87,88),])%>% st_cast("MULTIPOLYGON")
geomCAN$geometry[87] = HMM
geomCAN$HR_UID[87] = "473"
geomCAN$ENGNAME[87] = "Central"
#Merge Far North Central, West and East
HMM<- st_union(geomCAN[c(89,90,91),])%>% st_cast("MULTIPOLYGON")
geomCAN$geometry[89] = HMM
geomCAN$HR_UID[89] = "471"
geomCAN$ENGNAME[89] = "Far North"
#Merge North Central, West and East
HMM<- st_union(geomCAN[c(92,93,94),])%>% st_cast("MULTIPOLYGON")
geomCAN$geometry[92] = HMM
geomCAN$HR_UID[92] = "472"
geomCAN$ENGNAME[92] = "North"
#Merge South Central, West and East
HMM<- st_union(geomCAN[c(97,98,99),])%>% st_cast("MULTIPOLYGON")
geomCAN$geometry[97] = HMM
geomCAN$HR_UID[97] = "476"
geomCAN$ENGNAME[97] = "South"
#remove uneeded regions
geomCAN=geomCAN[-c(88,90,91,93,94,98,99),]


#integrate datasets
HAM = inner_join(geomCAN,CANADARISK,by = c("HR_UID" = "HR_UID"))
HAM$RegionName = paste0(HAM$health_region,", ",HAM$province,", Canada")
HAM$Country = "Canada"

CANADA_DATA = subset(HAM,select = c("DateReport","RegionName","Country","pInf","geometry"))
return(CANADA_DATA)
}
