#' tidy_Data
#'
#' @description For an object read in with a LoadCountry function, sets the per capita active cases (pInf) to NA or removes them from the dataset based on input conditions. Can be used to remove data that is not recent, or data that produces low or negative estimates of per capita cases, or risk. Can also be used to remove data that is NA by country or by region.
#'
#' @param DATA Data created using a LoadCountry call.
#' @param tidy If TRUE, then perform tidying according to other parameters. If FALSE, then do nothing.
#' @param DaysOld  Set any pInf data more than this days old to NA.
#' @param minimumpercapitaactivecases Set any pInf data less than this to NA.
#' @param RiskEval Set pInf to NA when risk is below RiskEval$minimumRisk (%) using RiskEval$ascertainmentbias and a maximum group size, RiskEval$maximumN (Note: this setting overwrites minimumpercapitaactivecases).
#' @param dropNACountry If TRUE, remove rows for countries whose pInf estimates all return NA.
#' @param dropNAall If TRUE, remove rows for any region whose pInf estimates all return NA
#'
#' @return Returns input simple feature with NA values for pInf according to input options.
#'
#' @examples
#' \dontrun{
#' Europe1 <- LoadEurope() #tidy_Data is run within LoadX functions.
#' Europe2 <- LoadEurope(tidy = FALSE)
#' Europe2 <- tidy_Data(Europe2)
#' }
#' @export
tidy_Data <- function(DATA, tidy = TRUE, DaysOld = 30, minimumpercapitaactivecases = 0, RiskEval = NULL, dropNACountry = TRUE, dropNAall = FALSE) {
  # DATA is a dataset created from a LoadX function.
  # Perform tidying only when tidy is TRUE.
  # Set any data older than DaysOld to Not Available.
  # Set any data where per capita active cases are below the minimumpercapitaactivecases to Not Available.
  # Set any data where Risk is below RiskEval$minimumRisk (%) using RiskEval$ascertainmentbias and a maximum group size, RiskEval$maximumN to Not Available. (Note: this setting overwrites minimumpercapitaactivecases).
  # remove rows for countries whose estimates all return NA
  # remove rows for any region whose estimates return NA
  
  #type checking
  stopifnot("`DATA` must have row entries." = nrow(DATA)>0)
  assertOptions__tidy_Data(tidy,DaysOld,minimumpercapitaactivecases,RiskEval,dropNACountry,dropNAall)
  
  if (tidy == TRUE) {

  ## set NA conditions:
  # 1. by report date
  if (!is.null(DaysOld) == TRUE) {
    DATA$pInf[which(as.Date(DATA$DateReport) < as.Date(Sys.Date() - DaysOld))] <- NA
  }

  # 2. by pInf value
  if (!is.null(minimumpercapitaactivecases) == TRUE || !is.null(RiskEval) == TRUE) {
    if (!is.null(RiskEval) == TRUE) {
      # Will set pInf to NA for a region if computed risk <= RiskEval$minimumRisk for the largest considered event size and ascertainment bias.
      minimumpercapitaactivecases <- (1 / RiskEval$ascertainmentbias) * (1 - (1 - RiskEval$minimumRisk / 100)^(1 / RiskEval$maximumN))
    }
    DATA$pInf[which(DATA$pInf <= minimumpercapitaactivecases)] <- NA
  }

  ## drop NA conditions:
  # 1. by country
  if (dropNACountry == TRUE) {
    CLIST <- unique(DATA$Country)
    REM_IND <- c()
    for (aa in 1:length(CLIST)) {
      CIND <- which(DATA$Country == CLIST[aa])
      isCallNA <- all(is.na(DATA$pInf[CIND]))
      if (isCallNA == TRUE) {
        REM_IND <- c(REM_IND, CIND)
      }
    }
    if (length(REM_IND) > 0){
      DATA <- DATA[-REM_IND, ]
    }
  }

  # 2. by NA
  if (dropNAall == TRUE) {
    DATA <- DATA[-which(is.na(DATA$pInf)), ]
  }

  }
  
  return(DATA)
}
