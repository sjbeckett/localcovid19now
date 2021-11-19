LoadCountries<-function(countries = NULL){

#COMBINE DATASETS INTO SINGLE OBJECT
NEWMAP = c()

if(is.null(countries)){
	countries = read.table("countries/countrylist.txt",header=FALSE,stringsAsFactor = FALSE)[[1]]
}

for(country in countries){
	#Load in data for this country
  cat("\n",country,"\n")
	try({
		this_country = get(country)()
		NEWMAP = rbind(NEWMAP,this_country)
	})
}

return(NEWMAP)
}