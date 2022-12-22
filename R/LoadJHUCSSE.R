#' LoadJHUCSSE
#'
#' @description Reads in subnational data for various regions in the JHU CSSE COVID-19 Dataset to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data Aggregated by the COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University \url{https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data}.
#'
#' @references
#' Dong, E., Du, H., & Gardner, L. (2020). An interactive web-based dashboard to track COVID-19 in real time. The Lancet infectious diseases, 20(5), 533-534.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' JH <- LoadJHUCSSE()
#' @seealso [LoadData()]
#' @export
LoadJHUCSSE <- function() {
  pop_smallcountries <- NULL
  utils::data("geomSmallCountries", envir = environment())
  utils::data("pop_smallcountries", envir = environment())
  geomSmallCountries <- sf::st_as_sf(geomSmallCountries)
  

  CountryList <- c("Singapore", "Brunei", "Djibouti", "Qatar", "Marshall Islands", "Saint Kitts and Nevis", "Timor-Leste", "Maldives", "Grenada", "Saint Vincent and the Grenadines", "Saint Lucia", "Barbados", "Antigua and Barbuda", "Seychelles", "Palau", "Micronesia", "Dominica", "Bahrain", "Kiribati", "Sao Tome and Principe", "Comoros", "Mauritius", "Samoa", "Trinidad and Tobago", "Lebanon", "Jamaica", "Gambia", "Vanuatu", "Bahamas", "Eswatini", "Kuwait", "Fiji", "El Salvador", "Belize", "Cabo Verde", "West Bank and Gaza")

  ProvinceList <- c("Falkland Islands (Malvinas)", "New Caledonia", "Turks and Caicos Islands", "Anguilla", "British Virgin Islands", "Bermuda", "Sint Maarten", "Aruba", "Curacao", "Cook Islands")

  # load cases data
  data <- vroom::vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", .name_repair = make.names, show_col_types = FALSE, progress = FALSE)

  # get updated date:
  date <- names(data)[length(names(data))]
  date <- strsplit(date, "X")[[1]][2]
  date <- as.Date(date, format = "%m.%d.%y")

  DateReport <- c()
  CaseDifference <- c()
  for (aa in 1:length(CountryList)) {
    subsetdata <- data[which(data$Country.Region == CountryList[aa]), ]
    DateReport[aa] <- as.character(date)
    CaseDifference[aa] <- (10 / 14) * diff(range(subsetdata[(length(subsetdata) - 14):(length(subsetdata))]))
  }
  LL <- aa

  for (aa in 1:length(ProvinceList)) {
    subsetdata <- data[which(data$Province.State == ProvinceList[aa]), ]
    DateReport[LL + aa] <- as.character(date)
    CaseDifference[LL + aa] <- (10 / 14) * diff(range(subsetdata[(length(subsetdata) - 14):(length(subsetdata))]))
  }

  caseTable <- data.frame(CountryList = c(CountryList, ProvinceList), DateReport, CaseDifference)
  caseTable$CountryList[which(caseTable$CountryList == "Sao Tome and Principe")] <- dplyr::pull(geomSmallCountries[geomSmallCountries$geoid == "STP678_00_00", ], "country_name") # São Tomé and Príncipe
  caseTable$CountryList[which(caseTable$CountryList == "Curacao")] <- dplyr::pull(geomSmallCountries[geomSmallCountries$geoid == "CUW531_00_00", ], "country_name") # Curaçao
  caseTable$CountryList[which(caseTable$CountryList == "Falkland Islands (Malvinas)")] <- "Falkland Islands"

  # Geography # from world bank https://datacatalog.worldbank.org/dataset/world-bank-official-boundaries

  geomSmallCountries <- dplyr::inner_join(geomSmallCountries, pop_smallcountries, by = c("country_name" = "NAME_EN"))

  geomSmallCountries$country_name[which(geomSmallCountries$country_name == "Federated States of Micronesia")] <- "Micronesia"
  geomSmallCountries$country_name[which(geomSmallCountries$country_name == "The Bahamas")] <- "Bahamas"
  geomSmallCountries$country_name[which(geomSmallCountries$country_name == "The Gambia")] <- "Gambia"
  geomSmallCountries$country_name[which(geomSmallCountries$country_name == "eSwatini")] <- "Eswatini"
  geomSmallCountries$country_name[which(geomSmallCountries$country_name == "East Timor")] <- "Timor-Leste"
  geomSmallCountries$country_name[which(geomSmallCountries$country_name == "Cape Verde")] <- "Cabo Verde"
  geomSmallCountries$country_name[which(geomSmallCountries$country_name == "Palestine")] <- "West Bank and Gaza"




  # integrate datasets
  MapGL <- dplyr::inner_join(geomSmallCountries, caseTable, by = c("country_name" = "CountryList"))
  MapGL$RegionName <- MapGL$country_name
  MapGL$Country <- MapGL$country_name
  MapGL$DateReport <- as.character(MapGL$DateReport)
  MapGL$pInf <- MapGL$CaseDifference / MapGL$POP_EST

  COUNTRIES <- subset(MapGL, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(COUNTRIES)
}
