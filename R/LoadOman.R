#' LoadOman
#'
#' @description Reads in subnational data for Oman to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Ministry of Health for Oman, collated by Safeture for the Humanitarian Data Exchange: \url{https://data.humdata.org/dataset/oman-coronavirus-covid-19-subnational-cases}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Oman <- LoadOman()
#' @seealso [LoadData()]
#' @export
LoadOman <- function() {
  # Ministry of Health for Oman, collated by Safeture for the Humanitarian Data Exchange: https://data.humdata.org/dataset/oman-coronavirus-covid-19-subnational-cases

  utils::data("geomOman", envir = environment())
  geomOman <- sf::st_as_sf(geomOman)

  CaseData <- utils::read.csv("https://www.dropbox.com/s/ylop7xswywi147c/cases_oman.csv?dl=1")
  Governorates <- sort(unique(CaseData$name))
  CaseDiff <- c()
  DateReport <- c()

  for (aa in 1:length(Governorates)) {
    subsetdata <- CaseData[which(CaseData$name == Governorates[aa]), ]
    CaseDiff[aa] <- (10 / 14) * sum(utils::tail(subsetdata$cases, 14))
    DateReport[aa] <- utils::tail(subsetdata$date, 1)
  }
  omandt <- data.frame(DateReport, Name = Governorates, Difference = CaseDiff)

  # population
  pop <- c()
  pop$Name <- c("Ad Dakhiliyah", "Al Buraymi", "Al Wusta", "Ad Dhahirah", "Al Batinah South", "Ash Sharqiyah South", "Muscat", "Musandam", "Al Batinah North", "Ash Sharqiyah North", "Dhofar")
  pop$Population <- c(478501, 121802, 52344, 213043, 465550, 315445, 1302440, 49062, 784681, 271822, 416458) # 2020 census - https://www.citypopulation.de/en/oman/cities/
  pop <- data.frame(pop)

  omandf <- dplyr::inner_join(omandt, pop, by = c("Name" = "Name"))

  # geometry
  # https://data.humdata.org/dataset/cod-ab-omn?
  # data("geomOman")
  geomOman$micro_name[5] <- "Ad Dhahirah"
  geomOman$micro_name[1] <- "Ad Dakhiliyah"

  OmanMap <- dplyr::inner_join(geomOman, omandf, by = c("micro_name" = "Name"))
  OmanMap$Country <- OmanMap$country_name
  OmanMap$RegionName <- paste(OmanMap$micro_name, OmanMap$country_name, sep = ", ")
  OmanMap$pInf <- as.numeric(OmanMap$Difference) / as.numeric(OmanMap$Population)
  Oman_DATA <- subset(OmanMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(Oman_DATA)
}
