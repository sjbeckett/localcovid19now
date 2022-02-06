LoadCountries <- function(countries = NULL) {

  # COMBINE DATASETS INTO SINGLE OBJECT
  NEWMAP <- c()

  if (is.null(countries)) {
    countries <- read.table("countries/countrylist.txt", header = FALSE, stringsAsFactor = FALSE)[[1]]
  }

  errors <<- tibble(countryn = c(), errort = c(), datetime = c())
  for (country in countries) {
    # Load in data for this country
    cat("\n", country, "\n")
    tryCatch(
      {
        this_country <- get(country)()
        NEWMAP <- bind_rows(NEWMAP, this_country)
      },
      error = function(cond) {
        errors <<- bind_rows(errors, tibble(countryn = country, errort = as.character(cond), datetime = now(tzone = "UTC")))
      }
    )
  }
  write.csv(errors, "log_error/errors.csv", append = T)

  return(NEWMAP)
}
