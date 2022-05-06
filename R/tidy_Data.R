#' tidy_Data
#'
#' @description For an object read in with a LoadCountry function, sets the per capita active cases (pInf) to NA when reports are over 30 days old, or if no, or negative, change in cumulative cases over the most recent 14 day period.
#'
#' @param DATA Data created using a LoadCountry call.
#'
#' @return Returns input simple feature with NA values for pInf when reports are over 30 days old or suggest that there is no, or negative, change in cumulative cases over the most recent 14 day period.
#'
#' @examples
#' \dontrun{
#' Philippines <- LoadPhilippines()
#' Philippines <- tidy_Data(Philippines)
#' }
#' @export
tidy_Data <- function(DATA){
#DATA is a dataset created from a LoadX function.
#Set any data older than a month to Not Available.
#Set any data where case differences are 0 or negative to Not Available.

DATA$pInf[which(as.Date(DATA$DateReport) < as.Date(Sys.Date()-30))] = NA
DATA$pInf[which(DATA$pInf<=0)] = NA

return(DATA)
}




tidy_Data <- function(DATA, DaysOld = 30, minimumpercapitaactivecases = 0, RiskEval = NULL, dropNACountry = FALSE, dropNAall = FALSE){
#DATA is a dataset created from a LoadX function.
#Set any data older than DaysOld to Not Available.
#Set any data where per capita active cases are below the minimumpercapitaactivecases to Not Available.
#Set any data where Risk is below RiskEval$minimumRisk (%) using RiskEval$ascertainmentbias and a maximum group size, RiskEval$maximumN to Not Available. (Note: this setting overwrites minimumpercapitaactivecases).
#remove rows for countries whose estimates all return NA
#remove rows for any region whose estimates return NA


  ##set NA conditions:
  #1. by report date
  if(!is.null(DaysOld)==TRUE){
	DATA$pInf[which(as.Date(DATA$DateReport) < as.Date(Sys.Date()-DaysOld))] = NA
  }

  #2. by pInf value
  if(!is.null(minimumpercapitaactivecases)==TRUE || !is.null(RiskEval)==TRUE){
	if(!is.null(RiskEval)==TRUE){
		#Will set pInf to NA for a region if computed risk <= RiskEval$minimumRisk for the largest considered event size and ascertainment bias.
		minimumpercapitaactivecases = (1/RiskEval$maxAscertainmentbias)*(1-(1-RiskEval$minimumRisk/100)^(1/RiskEval$maximumN))
	}
    DATA$pInf[which(DATA$pInf<= minimumpercapitaactivecases)] = NA
  }

  ##drop NA conditions:
  #1. by country
  if(dropNACountry == TRUE){
	CLIST = unique(DATA$Country)
	REM_IND = c()
	for(aa in 1:length(CLIST)){
		CIND  = which(DATA$Country == CLIST[aa])
		isCallNA = all(is.na(DATA$pInf[CIND]))
		if(isCallNA==TRUE){
			REM_IND = c(REM_IND,CIND)
		}
	}
	DATA = DATA[-REM_IND,]
  }
  
  #2. by NA
  if(dropNAall == TRUE){
	DATA = DATA[-which(is.na(DATA$pInf)),]
  }

return(DATA)
}