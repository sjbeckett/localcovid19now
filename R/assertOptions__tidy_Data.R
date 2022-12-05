#' assertOptions__tidy_Data
#'
#' @description Checks if inputs to tidy_Data are appropriate.
#'
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
#' assertOptions__tidy_Data(tidy = TRUE, DaysOld = 30, minimumpercapitaactivecases = 0, RiskEval = NULL, dropNACountry = TRUE, dropNAall = FALSE)
#' @export
assertOptions__tidy_Data <- function(tidy, DaysOld, minimumpercapitaactivecases, RiskEval, dropNACountry, dropNAall) {
#type checking
stopifnot("`tidy` must be a logical." = is.logical(tidy))
stopifnot("`dropNACountry` must be a logical." = is.logical(dropNACountry))
stopifnot("`dropNAall` must be a logical." = is.logical(dropNAall))
stopifnot("`DaysOld` must be numeric and positive." = is.numeric(DaysOld) & DaysOld > 0)
stopifnot("`minimumpercapitaactivecases` must be numeric and greater than or equal to 0." = is.numeric(minimumpercapitaactivecases) & minimumpercapitaactivecases >= 0)
stopifnot("`RiskEval` must be numeric and between 0 and 100 (%)." = is.numeric(RiskEval) & RiskEval >= 0 & RiskEval <= 100)
}