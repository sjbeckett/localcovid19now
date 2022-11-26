#' LoadCountries
#'
#' @param countries List of countries to collect data for. Use NULL to attempt to download all available datasets.
#' @param filepath Optionally, provide a filepath to save error file to.
#' @param interactiveMode Set whether the session is being run interactively. If not and no googledrive oauth token is found, avoid data requiring googledrive auth token.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#' @export
#'
#' @examples
#' AllData <- LoadCountries()
LoadCountries <- function(countries = NULL, filepath = NULL, interactiveMode = interactive()) {

  # COMBINE DATASETS INTO SINGLE OBJECT
  NEWMAP <- c()

  # assign country list if not provided
  if (is.null(countries)) {
    utils::data(countrylist, envir = environment())
    countries <- countrylist
  }

  # check whether googledrive credentials are available if in non-interactive mode.

  if (interactiveMode == FALSE) {
    TOKEN <- googledrive::drive_has_token()
    if (TOKEN == FALSE) {
      warning("No googledrive token found. Will avoid loading data requiring token. For more about providing authorization, see: https://gargle.r-lib.org/articles/non-interactive-auth.html")
      if ("LoadPhilippines" %in% countries) {
        countries <- countries[-which(countries == "LoadPhilippines")]
      }
    }
  }

  # initialize data frame to store error information
  errors <- data.frame(countryn = c(), errort = c(), datetime = c())

  for (country in countries) {
    # Load in data for this country
    cat(country, "\n")
    tryCatch(
      {
        this_country <- get(country)()
        NEWMAP <- dplyr::bind_rows(NEWMAP, this_country)
      },
      error = function(cond) {
        errors <- dplyr::bind_rows(errors, data.frame(countryn = country, errort = as.character(cond), datetime = lubridate::now(tzone = "UTC")))
      }
    )
  }

  if (!is.null(filepath)) { ## if filepath provided save errors to file
    utils::write.csv(errors, paste(filepath, ".csv"), append = T)
  }

  return(NEWMAP)
}
