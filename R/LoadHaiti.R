#' LoadHaiti
#'
#' @description Reads in subnational data for Haiti to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Released by the Ministry of Public Health and Population of Haiti to the Humanitarian Data Exchange: https://data.humdata.org/dataset/haiti-covid-19-subnational-cases.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Haiti <- LoadHaiti()
#' @seealso [LoadData()]
#' @export
LoadHaiti <- function() {
  # Released by the Ministry of Public Health and Population of Haiti to the Humanitarian Data Exchange: https://data.humdata.org/dataset/haiti-covid-19-subnational-cases.

  AA <- utils::read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTqVOxCSrhEiZ_CRME3Xqhu_DWZv74FvrvOr77rIXOlorClEi0huwVKxXXcVr2hn8pml82tlwmf59UX/pub?output=csv", skip = 1)
  names(AA) <- c("Date", "Name", "TotalCases", "NewCases", "TotalDeaths", "NewDeaths", "CFR", "Source")

  # format dates
  AA$Date <- as.character(as.Date(AA$Date, format = "%d-%m-%Y"))

  # format numerics
  AA$TotalCases <- as.numeric(gsub(",", "", AA$TotalCases))
  AA$NewCases <- as.numeric(gsub(",", "", AA$NewCases))
  AA$TotalDeaths <- as.numeric(gsub(",", "", AA$TotalDeaths))
  AA$NewDeaths <- as.numeric(gsub(",", "", AA$NewDeaths))

  # manual edits to correct duplicate locations
  AA$Name[which(AA$Name == "sud")] <- "Sud"
  AA$Name[which(AA$Name == "GrandAnse")] <- "Grand Anse"
  AA$Name[which(AA$Name == "Quest")] <- "Ouest"
  # align names with geomHaiti
  AA$Name[which(AA$Name == "Grand Anse")] <- "Grand'Anse"
  AA$Name[which(AA$Name == "Artibonite")] <- "L'Artibonite"

  LOCS <- unique(AA$Name)
  DateReport <- c()
  CaseDiff <- c()
  for (aa in 1:length(LOCS)) {
    subsetdata <- AA[which(AA$Name == LOCS[aa]), ]
    DateReport[aa] <- as.character(max(as.Date(subsetdata$Date)))
    CaseDiff[aa] <- (10 / 14) * (subsetdata$TotalCases[which(subsetdata$Date == DateReport[aa])] - subsetdata$TotalCases[which(as.Date(subsetdata$Date) == (as.Date(DateReport[aa])) - 14)])
  }

  Haitidf <- data.frame(DateReport, Location = LOCS, CaseDiff)

  # geometry
  utils::data("geomHaiti", envir = environment())
  geomHaiti <- sf::st_as_sf(geomHaiti)

  # population
  # from 2015 Haiti census https://web.archive.org/web/20151106110552/http://www.ihsi.ht/pdf/projection/Estimat_PopTotal_18ans_Menag2015.pdf https://en.wikipedia.org/wiki/Departments_of_Haiti
  Pop <- c()
  Pop$location <- c("L'Artibonite", "Centre", "Grand'Anse", "Nippes", "Nord", "Nord-Est", "Nord-Ouest", "Ouest", "Sud-Est", "Sud")
  Pop$population <- c(1727524, 746236, 468301, 342525, 1067177, 393967, 728807, 4029705, 632601, 774976)
  Pop <- data.frame(Pop)

  # join
  Haitidf2 <- dplyr::inner_join(Haitidf, Pop, by = c("Location" = "location"))
  HaitiMap <- dplyr::inner_join(geomHaiti, Haitidf2, by = c("micro_name" = "Location"))


  HaitiMap$RegionName <- paste(HaitiMap$micro_name, "Haiti", sep = ", ")
  HaitiMap$Country <- "Haiti"
  HaitiMap$pInf <- HaitiMap$CaseDiff / HaitiMap$population
  HAITI_DATA <- subset(HaitiMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(HAITI_DATA)
}
