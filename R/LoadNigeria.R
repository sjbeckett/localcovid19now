#' LoadNigeria
#'
#' @description Reads in subnational data for Nigeria to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' COVID-19 data collated by Humanitarian Emergency Response Africa  \url{https://data.humdata.org/dataset/nigeria_covid19_subnational} which is
#' sourced from data collected by Nigeria Centre for Disease Control and National Primary Health Care Development Agency.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Nigeria <- LoadNigeria()
#' @seealso [LoadData()]
#' @export
LoadNigeria <- function() {
  # Humanitarian Emergency Response Africa  https://data.humdata.org/dataset/nigeria_covid19_subnational
  # sourced from data collected by Nigeria Centre for Disease Control and National Primary Health Care Development Agency.
  # updated ~ weekly.

  geomNigeria <- NULL
  utils::data("geomNigeria", envir = environment())
  geomNigeria <- sf::st_as_sf(geomNigeria)

  casedata <- vroom::vroom("https://data.humdata.org/dataset/f5c35452-d766-468a-a272-4bd82d0a3be0/resource/15b9978f-422b-4b3d-9513-359b520d8352/download/nga_subnational_covid19_hera.csv", delim = ";", show_col_types = FALSE, progress = FALSE)

  regions <- unique(casedata$REGION)
  DateReport <- c()
  CaseDifference <- c()

  for (aa in 1:length(regions)) {
    subsetdata <- casedata[which(casedata$REGION == regions[aa]), ]
    CaseDates <- as.Date(subsetdata$DATE, format = "%d/%m/%Y")
    DateReport[aa] <- as.character(max(CaseDates))
    CaseDifference[aa] <- (10 / 14) * sum(utils::tail(subsetdata$CONTAMINES, 14))
  }

  caseTable <- data.frame(regions, DateReport, CaseDifference)

  # population data from 2016 census https://nigerianstat.gov.ng/download/775
  Population <- c()
  Population$Name <- c("Kano", "Lagos", "Kaduna", "Katsina", "Oyo", "Rivers", "Bauchi", "Jigawa", "Benue", "Anambra", "Borno", "Delta", "Niger", "Imo", "Akwa Ibom", "Ogun", "Sokoto", "Ondo", "Osun", "Kogi", "Zamfara", "Enugu", "Kebbi", "Edo", "Plateau", "Adamawa", "Cross River", "Abia", "Ekiti", "Kwara", "Gombe", "Yobe", "Taraba", "Ebonyi", "Nasarawa", "Bayelsa", "Federal Capital Territory")
  Population$population <- c(15076892, 12000598, 8252366, 7831319, 7840864, 7303924, 6537314, 5828163, 5741815, 5527809, 5860183, 5663362, 5556247, 5408756, 5482177, 5217716, 4998090, 4671695, 4705589, 4473490, 4515427, 4411119, 4440050, 4235595, 4200442, 4248436, 3866269, 3727347, 3270798, 3192893, 3256962, 3294137, 3066834, 2880383, 2523395, 2277961, 3564126)
  Population <- as.data.frame(Population)

  Nigeriadf <- dplyr::inner_join(caseTable, Population, by = c("regions" = "Name"))

  # geography
  # https://observablehq.com/@thehogfather/nigeria-geojson
  # geomNigeria = st_read("https://static.observableusercontent.com/files/4a1cdc776a6b3fe9c53bd903a0bc12928cdf1597cbba1d215492b0640d7624fe2c295fd2f9f509df66f7dcb1d1bd2b3760b7a68f2afb36bc2b3fe2ade8a80557?response-content-disposition=attachment%3Bfilename*%3DUTF-8%27%27nigeria_state_boundaries.geojson")
  # data("geomNigeria")

  # integrate datasets
  MapNigeria <- dplyr::inner_join(geomNigeria, Nigeriadf, by = c("micro_name" = "regions"))
  MapNigeria$RegionName <- paste(MapNigeria$micro_name, MapNigeria$country_name, sep = ", ")
  MapNigeria$Country <- MapNigeria$country_name
  MapNigeria$pInf <- MapNigeria$CaseDifference / MapNigeria$population

  NIGERIA_DATA <- subset(MapNigeria, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(NIGERIA_DATA)
}
