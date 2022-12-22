#' LoadData
#'
#' @description General framework for loading and tidying data.
#'
#' @param functionNames Name(s) of function representing that for loading data - options are in countrylist. Use NULL to attempt to download all available datasets.
#' @param filepath Optionally, provide a filepath to save an error csv file to.
#' @param interactiveMode Set whether the session is being run interactively. If not and no googledrive oauth token is found, avoid data requiring googledrive auth token.
#' @param tidy If TRUE, then perform tidying according to other parameters. If FALSE, then do nothing. (passed to tidy_Data).
#' @param DaysOld  Set any pInf data more than this days old to NA.(passed to tidy_Data).
#' @param minimumpercapitaactivecases Set any pInf data less than this to NA.(passed to tidy_Data).
#' @param RiskEval Set pInf to NA when risk is below RiskEval$minimumRisk (%) using RiskEval$ascertainmentbias and a maximum group size, RiskEval$maximumN (Note: this setting overwrites minimumpercapitaactivecases). (passed to tidy_Data).
#' @param dropNACountry If TRUE, remove rows for countries whose pInf estimates all return NA.(passed to tidy_Data).
#' @param dropNAall If TRUE, remove rows for any region whose pInf estimates all return NA. (passed to tidy_Data).
#' @param custom If TRUE, allows for use with functions not listed in object countrylist (experimental usage).
#' @param verbose If TRUE, reports on loading progress and returns warnings/errors to console.
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' LoadData("LoadUS")
#' LoadData("LoadUS", dropNAall = TRUE)
#' LoadData("LoadNewZealand",tidy = FALSE)
#' LoadData(c("LoadUS","LoadMalaysia"))
#' \dontrun{
#' LoadData()
#' }
#' @seealso [tidy_Data()]
#' @export
LoadData <- function(functionNames = NULL, custom = FALSE, filepath = NULL, interactiveMode = interactive(), tidy = TRUE, DaysOld = 30, minimumpercapitaactivecases = 0, RiskEval = NULL, dropNACountry = TRUE, dropNAall = FALSE, verbose = TRUE){
  
  #check inputs
  stopifnot("`custom` must be a logical." = is.logical(custom))
  stopifnot("`filepath` must be a character string, or be set to null." = is.character(filepath) || is.null(filepath))
  assertOptions__tidy_Data(tidy,DaysOld,minimumpercapitaactivecases,RiskEval,dropNACountry,dropNAall)
  stopifnot("`interactiveMode` must be a logical." = is.logical(interactiveMode))
  stopifnot("`verbose` must be a logical." = is.logical(verbose))

  
  # COMBINE DATASETS INTO SINGLE OBJECT
  NEWMAP <- c()
  
  utils::data(countrylist, envir = environment())
  
  # assign country list if not provided
  if (is.null(functionNames)) {
    functionNames <- countrylist
  }
  
  indcheck <- which(!(functionNames %in% countrylist))
  if(length(indcheck)>0){ #if not on main list, check on the alt_countrylist
      for(aa in indcheck){
        thisC <- which(localcovid19now::alt_countrylist$inputCase == functionNames[aa])
        if(length(thisC)>0){
          functionNames[aa] <- localcovid19now::alt_countrylist$loadingCall[thisC]
        }
      }
  }
  
  # remove possible duplicates
  functionNames = unique(functionNames)
  
  if (custom == FALSE){
    stopifnot("`functionNames` must match options in object countrylist, or be set to NULL." = all(functionNames %in% countrylist))
  }
  
  
  #check googledrive permissions if trying to load using this package.
  if ("LoadPhilippines" %in% functionNames) {
    if (interactiveMode == FALSE) {
      TOKEN <- googledrive::drive_has_token()
      if (TOKEN == FALSE) {
        warning("No googledrive token found. Will avoid loading data requiring token.\nFor more about providing authorization, see: https://gargle.r-lib.org/articles/non-interactive-auth.html \nor run googledrive::drive_auth().")
        functionNames <- functionNames[-which(functionNames == "LoadPhilippines")]
      }
    } else {
      check_gdrive_scope()
    }
  }
  
  
  
  # initialize data frame to store error/warning information
  errors <- data.frame(countryn = c(), errort = c(), datetime = c())
  
  # load datasets
  for (country in functionNames) {
    if (verbose == TRUE) {
      cat(paste(country,"\n"))
    }
    
    tryCatch({
      DataLoader <- get(country)
      DATA <- DataLoader()
      NEWMAP <- dplyr::bind_rows(NEWMAP, DATA)
    },
    error = function(cond) {
      errors <<- dplyr::bind_rows(errors, data.frame(countryn = country, errort = as.character(cond), datetime = lubridate::now(tzone = "UTC")))
      if ( verbose == TRUE) {
        LoadErrorWarning(country)
      }
    })
  }

  
  #check if there is output, and if so perform tidying.
  if (length(NEWMAP) == 0){
    message("NOTE: unfortunately no data was able to be loaded.")
  } else {	
    
    # tidy data
    NEWMAP <- tidy_Data(NEWMAP, tidy, DaysOld, minimumpercapitaactivecases, RiskEval, dropNACountry, dropNAall)
    
    if (nrow(NEWMAP) == 0){
      message("NOTE: tidy_Data options removed all rows from the data object returned.")
    }
  }
  
  # if filepath provided save errors to file
  if (!is.null(filepath)) { 
    print(errors)
    utils::write.csv(errors, paste0(filepath, ".csv"))
  }
  
  return(NEWMAP)	
}