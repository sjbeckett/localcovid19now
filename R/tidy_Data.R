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