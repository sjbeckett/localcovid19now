#' LoadCountries
#'
#' @param countries List of countries to collect data for. Use NULL to attempt to download all available datasets.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#' @export
#'
#' @examples
#' \dontrun{
#' AllData = LoadCountries()
#' }
LoadCountries <- function(countries = NULL) {

  # COMBINE DATASETS INTO SINGLE OBJECT
  NEWMAP <- c()

  if (is.null(countries)) {
    countries <- countrylist
  }

  errors <<- data.frame(countryn = c(), errort = c(), datetime = c())
  for (country in countries) {
    # Load in data for this country
    cat("\n", country, "\n")
    tryCatch(
      {
        this_country <- get(country)()
        NEWMAP <- dplyr::bind_rows(NEWMAP, this_country)
      },
      error = function(cond) {
        errors <<- dplyr::bind_rows(errors, data.frame(countryn = country, errort = as.character(cond), datetime = lubridate::now(tzone = "UTC")))
      }
    )
  }
  write.csv(errors, "log_error/errors.csv", append = T)

  return(NEWMAP)
}
