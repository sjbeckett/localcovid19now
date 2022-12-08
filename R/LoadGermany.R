#' LoadGermany
#'
#' @description Reads in subnational data for Germany to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' COVID-19 case data for Germany is from the Robert Koch-Institut and the Bundesamt für Kartographie und Geodäsie: \url{https://npgeo-corona-npgeo-de.hub.arcgis.com/} \url{https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/about}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Germany <- LoadGermany()
#' }
#' @seealso [LoadData()]
#' @export
LoadGermany <- function() {
  Landkreis <- AnzahlFall <- Meldedatum <- IdLandkreis <- Date <- Region <- Cases <- latestDate <- CumSum <- pastDate <- difference <- NULL
  # Load in geometry and population
  utils::data("geomGermany", envir = environment())
  geomGermany <- sf::st_as_sf(geomGermany)
  utils::data("pop_germany", envir = environment())

  # COVID-19 case data for Germany is from the Robert Koch-Institut and the Bundesamt für Kartographie und Geodäsie: https://npgeo-corona-npgeo-de.hub.arcgis.com/ https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/about .
  links <- c(
    "https://opendata.arcgis.com/datasets/8a0b7d7c9fb442ffaa512221cf11366e_0.csv", # Baden-Württemberg
    "https://opendata.arcgis.com/datasets/45258e51f57d43efb612f700a876ae8f_0.csv", # bayern
    "https://opendata.arcgis.com/datasets/3949d6fd2dc74386b763e451f4c6e384_0.csv", # Berlin
    "https://opendata.arcgis.com/datasets/5f81692e203a4888a64cb1976aafbd34_0.csv", # Brandenburg
    "https://opendata.arcgis.com/datasets/f7bdcbe7188545daabe65e6c9e2a4379_0.csv", # Bremen
    "https://opendata.arcgis.com/datasets/ab2c1b9c36734faf937cd83dee339517_0.csv", # Hamburg
    "https://opendata.arcgis.com/datasets/3ed997d4a8a447f09ab122a1a432b070_0.csv", # Hessen
    "https://opendata.arcgis.com/datasets/d6c27576ee034bb78621012738615598_0.csv", # Mecklenburg-Vorpommern
    "https://opendata.arcgis.com/datasets/14d82a9addf841789cd6ef5c1f67476a_0.csv", # Niedersachsen/Lower Saxony
    "https://opendata.arcgis.com/datasets/a99afefd4258435f8af660b6cbed9bf7_0.csv", # Nordrhein-Westfalen
    "https://opendata.arcgis.com/datasets/57e385f51a07495cb0a1e00a55ee1b5b_0.csv", # Rheinland-Pfalz
    "https://opendata.arcgis.com/datasets/0e59e1262dba4f5f8d6a904113bf7c99_0.csv", # Saarland
    "https://opendata.arcgis.com/datasets/3d3235c08d4f44a2afd088546b704902_0.csv", # Sachsen
    "https://opendata.arcgis.com/datasets/06a1c943a9b845968b5ad0607f5f48f5_0.csv", # Sachsen-Anhalt
    "https://opendata.arcgis.com/datasets/4a648483aedd49b8a6655290181d4c2a_0.csv", # Schleswig-Holstein
    "https://opendata.arcgis.com/datasets/790f5423e03e49c4baec55a1a232c136_0.csv" # Thüringen
  ) # None of these are  spatial data, so it doesn't make sense to use geojsons.
  germanyData <- purrr::map_df(
    links,
    ~ vroom::vroom(.x, show_col_types = FALSE, progress = FALSE) %>%
      dplyr::select(Region = Landkreis, Cases = AnzahlFall, Date = Meldedatum, IdLandkreis)
  )
  germanyData <- germanyData %>%
    dplyr::mutate(
      Date = lubridate::as_date(Date),
    ) %>%
    dplyr::arrange(Date) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(
      CumSum = cumsum(Cases)
    )

  germanyTable <- germanyData %>%
    dplyr::summarise(
      latestDate = dplyr::last(Date),
      pastDate = dplyr::last(Date[Date <= (latestDate - 14)]), # there are some missing days in the past, so we have to choose the close day
      difference = ((max(CumSum) - max(CumSum[Date == pastDate])) * 10 / as.numeric(latestDate - pastDate)), # find the difference based on the real pastDate and the difference of days between latest and past
      IdLandkreis = unique(IdLandkreis)
    ) %>%
    dplyr::select(
      IdLandkreis,
      Region,
      Date = latestDate,
      Difference = difference
    )

  ## pop_germany: https://www.citypopulation.de/en/germany/admin/


  # SK Eisenach (16056) is reported with LK Wartburgkreis (16063) (according to RKI)
  pop_germany$Population[which(pop_germany$IdLandkreis == "16063")] <- pop_germany$Population[which(pop_germany$IdLandkreis == "16056")] + pop_germany$Population[which(pop_germany$IdLandkreis == "16063")]

  # Geometry
  # geomGermany <- st_read('https://public.opendatasoft.com/explore/dataset/covid-19-germany-landkreise/download/?format=geojson&timezone=Europe/Berlin&lang=en')

  # integrate datasets
  germanydf <- dplyr::inner_join(germanyTable, pop_germany, by = "IdLandkreis") # add pop_germany column into the main germany table
  germanyMap <- dplyr::inner_join(geomGermany, germanydf, by = c("micro_name" = "Region")) # link to geometry

  germanyMap$DateReport <- as.character(germanyMap$Date)
  germanyMap$RegionName <- paste(germanyMap$micro_name, germanyMap$country_name, sep = ", ")
  germanyMap$Country <- germanyMap$country_name
  germanyMap$pInf <- germanyMap$Difference / germanyMap$Population
  GERMANY_DATA <- subset(germanyMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(GERMANY_DATA)
}
