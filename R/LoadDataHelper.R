#' LoadDataHelper
#'
#' @description General framework for loading and tidying data.
#'
#' @param functionName Name of function representing that for loading data.
#' @param tidy If TRUE, then perform tidying according to other parameters. If FALSE, then do nothing. (passed to tidy_Data).
#' @param DaysOld  Set any pInf data more than this days old to NA.(passed to tidy_Data).
#' @param minimumpercapitaactivecases Set any pInf data less than this to NA.(passed to tidy_Data).
#' @param RiskEval Set pInf to NA when risk is below RiskEval$minimumRisk (%) using RiskEval$ascertainmentbias and a maximum group size, RiskEval$maximumN (Note: this setting overwrites minimumpercapitaactivecases). (passed to tidy_Data).
#' @param dropNACountry If TRUE, remove rows for countries whose pInf estimates all return NA.(passed to tidy_Data).
#' @param dropNAall If TRUE, remove rows for any region whose pInf estimates all return NA. (passed to tidy_Data).
#' @keywords internal
#' @return nothing.
#'
#' @examples
#' LoadDataHelper("LoadUS")
#' 
#' RE = c()
#' RE$minimumRisk = 5
#' RE$ascertainmentbias = 5
#' RE$maximumN = 100
#' LoadDataHelper("LoadUS", RiskEval=RE)
#' @export
LoadDataHelper <- function(functionName, tidy = TRUE, DaysOld = 30, minimumpercapitaactivecases = 0, RiskEval = NULL, dropNACountry = TRUE, dropNAall = FALSE){
  
  assertOptions__tidy_Data(tidy,DaysOld,minimumpercapitaactivecases,RiskEval,dropNACountry,dropNAall)
  
  tryCatch({
    DataLoader <- get(functionName)
    DATA <- DataLoader()
    DATA <- tidy_Data(DATA,tidy,DaysOld,minimumpercapitaactivecases,RiskEval,dropNACountry,dropNAall)
    if (nrow(DATA) == 0){
      message(paste0("NOTE: tidy_Data options removed all rows from the data object returned by ", functionName,"."))
    }
    return(DATA)
  }, warning = function(w){LoadErrorWarning(functionName)}
  )
}