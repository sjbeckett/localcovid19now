#' LoadFrance
#'
#' @description Reads in subnational data for France to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Santé publique France COVID-19 data for France :  \url{https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/} // \url{https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-a-compter-du-18-05-2022-si-dep/}.
#' Note this resource also contains data for overseas departments of France, and for Saint Barthélemy, Saint Martin, and Saint Pierre and Miquelon.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' France <- LoadFrance()
#' @seealso [LoadData()]
#' @export
LoadFrance <- function() {
  # Santé publique France COVID-19 data for France :  https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/ // https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-a-compter-du-18-05-2022-si-dep/
  # Note this resource also contains data for overseas departments of France, and for Saint Barthélemy, Saint Martin, and Saint Pierre and Miquelon.
  pop <- geomFrance <- NULL
  utils::data("geomFrance", envir = environment())
  geomFrance <- sf::st_as_sf(geomFrance)


  Code <- Department <- Population <- dep <- jour <- P <- cl_age90 <- cases <- NULL

  # sp-dep-day-2022-05-25-19h01.csv (this is daily data), from here:
  # https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-a-compter-du-18-05-2022-si-dep/#resources
  data <- vroom::vroom("https://www.data.gouv.fr/fr/datasets/r/426bab53-e3f5-4c6a-9d54-dba4442b3dbc", col_types = c("cDccccccc"), show_col_types = FALSE, progress = FALSE) %>%
    dplyr::filter(cl_age90 == 0) %>%
    dplyr::select(code = dep, date = jour, cases = P, Population = pop) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::filter(!is.na(cases))

  # replace ',' with . and convert to numeric
  data$cases <- as.numeric(gsub(",", ".", data$cases))
  data$Population <- as.numeric(gsub(",", ".", data$Population))

  # geom <<- st_read('https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-avec-outre-mer.geojson')
  # does not contain Saint Barthélemy, Saint Martin, and Saint Pierre and Miquelon.

  depList <- unique(data$code) # get the list of all department codes

  # sort out and calculate the number of cases during two recent weeks
  # depList[code] = corresponding code of a department
  sortFunc <- function(code) {
    deptCode <- depList[code]
    department <- data %>%
      dplyr::filter(code == deptCode) %>%
      dplyr::distinct(date, .keep_all = TRUE)
    latestDate <- department$date[length(department$date)]
    pastDate <- latestDate - 14
    difference <- (sum(department[1:which(department$date == latestDate), "cases"]) - sum(department[1:which(department$date == pastDate), "cases"])) / 14 * 10
    Population <- department$Population[1]
    vec <- data.frame(code = depList[code], date = latestDate, n = difference, pop = Population)
    return(vec)
  }

  # get the data table that includes department codes, last updated date, difference between 14 days
  frenchTable <- data.frame()
  for (i in 1:length(depList)) {
    vec <- sortFunc(i)
    frenchTable <- rbind(frenchTable, vec)
  }

  frenchdf <- frenchTable
  FranceMap <- dplyr::inner_join(geomFrance, frenchdf, by = c("micro_code" = "code"))
  FranceMap$RegionName <- paste(FranceMap$micro_name, FranceMap$country_name, sep = ", ")
  FranceMap$Country <- FranceMap$country_name
  FranceMap$DateReport <- as.character(FranceMap$date)
  FranceMap$pInf <- FranceMap$n / FranceMap$pop

  FRANCE_DATA <- subset(FranceMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(FRANCE_DATA)
}
