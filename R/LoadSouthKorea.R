#' LoadSouthKorea
#'
#' @description Reads in subnational data for South Korea to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data collated by \url{https://github.com/staedi/nCOV-summary/} from the South Korea CDC: \url{http://ncov.mohw.go.kr/}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' SouthKorea <- LoadSouthKorea()
#' }
#' @seealso [LoadData()]
#' @export
LoadSouthKorea <- function() {
  # collated by https://github.com/staedi/nCOV-summary/ from the South Korea CDC: http://ncov.mohw.go.kr/

  geomSouthKorea <- misc_southkorea <- NULL
  utils::data("geomSouthKorea", envir = environment())
  utils::data("misc_southkorea", envir = environment())
  geomSouthKorea <- sf::st_as_sf(geomSouthKorea)
  
  data <- utils::read.csv("https://raw.githubusercontent.com/staedi/nCOV-summary/master/time_series_covid19_infections.csv")
  SKOR <- data[which(data$adm0_a3 == "KOR"), ]

  Provinces <- unique(SKOR$Province.State)
  Population <- c()
  CaseDiff <- c()
  DateReport <- c()
  for (aa in 1:length(Provinces)) {
    subsetdata <- SKOR[which(SKOR$Province.State == Provinces[aa]), ]
    Population[aa] <- subsetdata$Population[1]
    CaseDiff[aa] <- (10 / 14) * diff(range(utils::tail(subsetdata$confirmed, 14)))
    DateReport[aa] <- as.character(as.Date(utils::tail(subsetdata$Date, 1), format = "%m/%d/%y"))
  }

  sk_df <- data.frame(DateReport, Provinces, CaseDiff, Population)

  # geometry
  # https://maps.princeton.edu/catalog/stanford-dk009rq9138
  # geomSouthKorea = stwrite(SK2,"geomSouthKorea.geojson")
  # data("geomSouthKorea")
  # get data for misc_southkoreaernative label
  # data("misc_southkorea")

  SouthKoreaMap <- dplyr::inner_join(geomSouthKorea, sk_df, by = c("micro_name" = "Provinces"))
  SouthKoreaMap <- dplyr::inner_join(SouthKoreaMap, misc_southkorea, by = c("micro_name" = "name_1"))
  Regions <- paste0(SouthKoreaMap$nl_name_1, "/", SouthKoreaMap$micro_name)

  SouthKoreaMap$Country <- SouthKoreaMap$country_name
  SouthKoreaMap$RegionName <- paste(Regions, SouthKoreaMap$country_name, sep = ", ")
  SouthKoreaMap$pInf <- as.numeric(SouthKoreaMap$CaseDiff) / as.numeric(SouthKoreaMap$Population)
  SouthKorea_DATA <- subset(SouthKoreaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(SouthKorea_DATA)
}
