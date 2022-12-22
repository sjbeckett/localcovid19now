#' LoadNewZealand
#'
#' @description Reads in subnational data for New Zealand to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' COVID-19 Data Repository by the Institute of Environmental Science and Research: \url{https://github.com/ESR-NZ/NZ_COVID19_Data/}
#' Institute for Environmental Science and Research, New Zealand COVID19 Dashboard. \url{https://nzcoviddashboard.esr.cri.nz/} (2020).
#'
#' @references
#' Jefferies, Sarah, et al. "COVID-19 in New Zealand and the impact of the national response: a descriptive epidemiological study." The Lancet Public Health 5.11 (2020): e612-e623.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' NewZealand <- LoadNewZealand()
#' @seealso [LoadData()]
#' @export
LoadNewZealand <- function() {
  # COVID-19 Data Repository by the Institute of Environmental Science and Research: https://github.com/ESR-NZ/NZ_COVID19_Data/
  # Institute for Environmental Science and Research, New Zealand COVID19 Dashboard. https://nzcoviddashboard.esr.cri.nz/ (2020).
  # Jefferies, Sarah, et al. "COVID-19 in New Zealand and the impact of the national response: a descriptive epidemiological study." The Lancet Public Health 5.11 (2020): e612-e623.

  utils::data("geomNZ", envir = environment())
  geomNZ <- sf::st_as_sf(geomNZ)

  # New Zealand's dashboard and system is currently down due to a system upgrade, hence why this isn't finding anything.

  # load case data

  # find list of files recorded in the github folder NZ_COVID19_Data/overview_case at https://github.com/ESR-NZ/NZ_COVID19_Data/tree/master/overview_case
  AA <- jsonlite::read_json("https://api.github.com/repos/ESR-NZ/NZ_COVID19_Data/git/trees/ceaec305a9bbfef50f7c9e90a029db7c123fa97f")
  lastfile_str <- AA$tree[[length(AA$tree)]]$path
  STRING <- paste0("https://github.com/ESR-NZ/NZ_COVID19_Data/raw/master/overview_case/", lastfile_str)
  NZ <- vroom::vroom(STRING, show_col_types = FALSE, progress = FALSE)


  Regions <- unique(NZ$DHBName)
  DateReport <- c()
  DateRecent <- c()
  CaseDifference <- c()

  for (aa in 1:length(Regions)) {
    subsetdata <- NZ[which(NZ$DHBName == Regions[aa]), ]
    DateRecent[aa] <- max(subsetdata$ReportDate)
    eligibleC <- subsetdata$Confirmed[which(lubridate::as_date(subsetdata$ReportDate) > lubridate::as_date(DateRecent[aa]) - 14)]
    CaseDifference[aa] <- (10 / 14) * sum(eligibleC)
    DateReport[aa] <- as.character(lubridate::as_date(DateRecent[aa]))
  }

  CaseTable <- data.frame(Regions, DateReport, CaseDifference)

  # population for 2020.
  # http://nzdotstat.stats.govt.nz/wbos/Index.aspx?DataSetCode=TABLECODE7509
  Pop <- c()
  Pop$Place <- c("Northland", "Waitemata", "Auckland", "Counties Manukau", "Waikato", "Lakes", "Bay of Plenty", "Tairawhiti", "Taranaki", "Hawke's Bay", "Whanganui", "MidCentral", "Hutt Valley", "Capital and Coast", "Wairarapa", "Nelson Marlborough", "West Coast", "Canterbury", "South Canterbury", "Southern")
  Pop$population <- c(172100, 575100, 472800, 522500, 393000, 106800, 226100, 48100, 116800, 163600, 64000, 173600, 146700, 302500, 44000, 147400, 32800, 525700, 58800, 316900)
  Pop <- as.data.frame(Pop)

  NZdf <- dplyr::inner_join(CaseTable, Pop, by = c("Regions" = "Place"))


  # geography
  # District health board regions https://datafinder.stats.govt.nz/layer/87883-district-health-board-2015/
  # https://github.com/leon-sleepinglion/NZ-COVID-19-Visualization
  # geomNZ = st_read("https://github.com/leon-sleepinglion/NZ-COVID-19-Visualization/raw/master/dhb.geojson")
  # data("geomNZ")
  Name2Move <- geomNZ$micro_name[8]
  geomNZ$micro_name[8] <- "Tairawhiti"

  # integrate datasets
  MapNZ <- dplyr::inner_join(geomNZ, NZdf, by = c("micro_name" = "Regions"))
  MapNZ$DateReport <- as.character(MapNZ$DateReport)
  MapNZ$micro_name[8] <- Name2Move
  MapNZ$RegionName <- paste(MapNZ$micro_name, MapNZ$country_name, sep = ", ")
  MapNZ$Country <- MapNZ$country_name
  MapNZ$pInf <- MapNZ$CaseDifference / MapNZ$population

  NEWZEALAND_DATA <- subset(MapNZ, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(NEWZEALAND_DATA)
}
