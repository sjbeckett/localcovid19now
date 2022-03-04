LoadCountriesProfile <- function(countries = NULL) {

  # COMBINE DATASETS INTO SINGLE OBJECT
  NEWMAP <- c()

  if (is.null(countries)) {
    countries <- read.table("countries/countrylist.txt", header = FALSE, stringsAsFactor = FALSE)[[1]]
  }

  TABLE <- c()

  for (country in countries) {
    # Load in data for this country
    print(country)
    TIME <- rep(NA, 5)

    try({
      ALLOCS <- profmem({
        TIME <- system.time({
          this_country <- get(country)()
        })
      })
      MEM <- sum(ALLOCS$bytes, na.rm = TRUE)
      MAXMEM <- max(ALLOCS$bytes, na.rm = TRUE)
      TABLE <- rbind(TABLE, c(script = country, totalbytes = MEM, maxbytes = MAXMEM, TIME))

      NEWMAP <- rbind(NEWMAP, this_country)
    })
  }

  write.csv(TABLE, paste0("Profile_", Sys.Date(), ".csv"), row.names = FALSE)

  return(NEWMAP)
}
