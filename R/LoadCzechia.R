#' LoadCzechia
#'
#' @description Reads in subnational data for Czechia to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' COVID-19 data sourced from National Health Information System, Regional Hygiene Stations, Ministry of Health of the Czech Republic and prepared by the Institute of Health Information and Statistics of the Czech Republic and the Institute of Biostatistics and Analyses, Faculty of Medicine, Masaryk University: \url{https://onemocneni-aktualne.mzcr.cz/covid-19}.
#'
#' @references
#' Komenda M., Karolyi M., Bulhart V., Žofka J., Brauner T., Hak J., Jarkovský J., Mužík J., Blaha M., Kubát J., Klimeš D., Langhammer P., Danková Š ., Májek O., Bartunková M., Dušek L. COVID 19: Overview of the current situation in the Czech Republic. Disease currently online. Prague: Ministry of Health of the Czech Republic, 2020. Available from: \url{https://onemocneni-aktualne.mzcr.cz/covid-19 }. Development: joint workplace of IHIS CR and IBA LF MU. ISSN 2694-9423.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Czechia <- LoadCzechia()
#' @seealso [LoadData()]
#' @export
LoadCzechia <- function() {
  # COVID-19 data sourced from National Health Information System, Regional Hygiene Stations, Ministry of Health of the Czech Republic and prepared by the Institute of Health Information and Statistics of the Czech Republic and the Institute of Biostatistics and Analyses, Faculty of Medicine, Masaryk University: https://onemocneni-aktualne.mzcr.cz/covid-19

  # Komenda M., Karolyi M., Bulhart V., Žofka J., Brauner T., Hak J., Jarkovský J., Mužík J., Blaha M., Kubát J., Klimeš D., Langhammer P., Danková Š ., Májek O., Bartunková M., Dušek L. COVID 19: Overview of the current situation in the Czech Republic. Disease currently [online]. Prague: Ministry of Health of the Czech Republic, 2020. Available from: https://onemocneni-aktualne.mzcr.cz/covid-19 . Development: joint workplace of IHIS CR and IBA LF MU. ISSN 2694-9423.

  pop_czechia <- geomCzechia <- NULL
  utils::data(list = c("geomCzechia", "pop_czechia"), envir = environment())
  geomCzechia <- sf::st_as_sf(geomCzechia)
  
  District <- Confirmed <- Date <- NULL

  # case data
  czechData <- vroom::vroom("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/kraj-okres-nakazeni-vyleceni-umrti.csv", show_col_types = FALSE, progress = FALSE)

  names(czechData) <- c("id", "Date", "Code", "District", "Confirmed", "Cure", "Death")

  czechData$Date <- as.Date(czechData$Date)
  czechData <- czechData %>%
    dplyr::group_by(District) %>%
    dplyr::slice(c(dplyr::n(), dplyr::n() - 14)) %>%
    dplyr::summarize(cases = (Confirmed[1] - Confirmed[2]) * 10 / 14, Date = dplyr::first(Date)) %>%
    dplyr::ungroup()

  # integrate datasets
  czech_data_join <- dplyr::inner_join(as.data.frame(czechData), pop_czechia, by = c("District" = "Code"))
  names(czech_data_join) <- c("Code", "Difference", "Date", "name", "Population")
  CzechMap <- dplyr::inner_join(geomCzechia, czech_data_join, by = c("micro_name" = "name"))

  CzechMap$RegionName <- paste(CzechMap$micro_name, CzechMap$country_name, sep = ", ")

  CzechMap$Country <- CzechMap$country_name
  CzechMap$DateReport <- as.character(CzechMap$Date)
  CzechMap$pInf <- CzechMap$Difference / CzechMap$Population

  CZECH_DATA <- subset(CzechMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(CZECH_DATA)
}
