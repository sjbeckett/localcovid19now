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
#' assertOptions__tidy_Data(TRUE, 30, 0, NULL, TRUE, FALSE)
#' @export
assertOptions__tidy_Data <- function(tidy, DaysOld, minimumpercapitaactivecases, RiskEval, dropNACountry, dropNAall) {
#type checking
stopifnot("`tidy` must be a logical." = is.logical(tidy))
stopifnot("`dropNACountry` must be a logical." = is.logical(dropNACountry))
stopifnot("`dropNAall` must be a logical." = is.logical(dropNAall))
stopifnot("`DaysOld` must be numeric and positive." = is.numeric(DaysOld) & DaysOld > 0)
stopifnot("`minimumpercapitaactivecases` must be numeric and greater than or equal to 0." = is.numeric(minimumpercapitaactivecases) & minimumpercapitaactivecases >= 0)
stopifnot("`RiskEval` must contain RiskEval$minimumRisk, RiskEval$ascertainmentbias and RiskEval$maximumN" = is.null(RiskEval) || (!is.null(RiskEval$minimumRisk) & !is.null(RiskEval$ascertainmentbias) & !is.null(RiskEval$maximumN)))
stopifnot("`RiskEval$minimumRisk` must be numeric and between 0 and 100 (%)." = is.numeric(RiskEval$minimumRisk) & RiskEval$minimumRisk >= 0 & RiskEval$minimumRisk <= 100)
stopifnot("`RiskEval$ascertainmentbias` must be numeric and positive." = is.numeric(RiskEval$ascertainmentbias) & RiskEval$ascertainmentbias > 0)
stopifnot("`RiskEval$maximumN' must be numeric and positive." = is.numeric(RiskEval$maximumN) & RiskEval$maximumN > 0)
}