#' makeMaps
#'
#' @description
#' Creates a whole world risk map and saves to PNG
#'
#' @export
#'
#' @examples
#' \dontrun{
#' makeMaps()
#' }
#'
makeMaps <- function() {

  # source local files
  # source country by country files
  country_source_files <- list.files("./countries/", pattern = "*.R", full.names = T)
  sapply(country_source_files, \(x) source(x, encoding = "UTF-8"))
  # source mapping utilities

  ### LOAD IN DATA

  # COMBINE DATASETS INTO SINGLE OBJECT
  ## Note: a GoogleDrive oauth is required for the LoadPhilippines() function. It will prompt for the filepath
  GLOBALMAP <- LoadCountries()

  # Create world_risk_regions.csv
  ## risk < 1% -> 0
  GLOBALDAT <- sf::st_drop_geometry(GLOBALMAP) %>%
    dplyr::as_tibble()
  create_c19r_data(df_in = GLOBALDAT)

  ### Append ascertainment bias calculations to each region via country level statistics
  GLOBALMAP$AB <- 3

  # today's date
  filedate <- paste(lubridate::day(lubridate::today()), lubridate::month(lubridate::today(), label = T, abbr = F), lubridate::year(lubridate::today()), sep = "")
  # save map
  sf::st_write(GLOBALMAP, sprintf("GlobalRiskMapping_ABD_%s.geojson", filedate), delete_dsn = T)

  # No data if pInf <= 0
  GLOBALMAP$pInf[which(GLOBALMAP$pInf <= 0)] <- NA
  # Subset to only include recent data
  GLOBALMAP$pInf[which(GLOBALMAP$DateReport < (Sys.Date() - 30))] <- NA

  # Map with 50 people
  MapTogether <- EventMap_leaflet(GLOBALMAP, 50)
  if (interactive()) {
    MapTogether
  }


  # Provides a csv of missing data for issue identification
  GLOBALMAP %>%
    dplyr::filter(is.na(pInf)) %>%
    sf::st_drop_geometry() %>%
    utils::write.csv(file = sprintf("log_error/pInfNA_%s.csv", filedate), row.names = F)

  htmlwidgets::saveWidget(MapTogether, sprintf("GlobalRiskMapping_ABD_50_%s.html", filedate), selfcontained = T)


  # Example static maps via tmap
  PCM <- PerCapitaMap_tmap(GLOBALMAP, 100000) # active cases per 100,000
  tmap::tmap_save(PCM, sprintf("Global_pcm_per100000_%s.png", filedate))
  EM <- EventMap_tmap(GLOBALMAP, 100) # risk for event of 100 people
  tmap::tmap_save(EM, sprintf("Global_RiskMap_Ev100_%s.png", filedate))
}
