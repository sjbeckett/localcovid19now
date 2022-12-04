#' LoadSweden
#'
#' @description Reads in subnational data for Sweden to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Swedish COVID-19 National Statistics from Folkh?lsomyndigheten: \url{https://experience.arcgis.com/experience/09f821667ce64bf7be6f9f87457ed9aa/page/page_0/}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Sweden <- LoadSweden()
#' @seealso [LoadData()]
#' @export
LoadSweden <- function() {
  Totalt_antal_fall <- County <- cases <- pop_sweden <- cases_past <- date_past <- NULL

  # Load in geometry and population data
  utils::data("geomSweden", envir = environment())
  utils::data("pop_sweden", envir = environment())
  geomSweden <- sf::st_as_sf(geomSweden)

  # Swedish COVID-19 National Statistics from Folkh?lsomyndigheten: https://experience.arcgis.com/experience/09f821667ce64bf7be6f9f87457ed9aa/page/page_0/
  temp <- tempfile()
  utils::download.file(url = "https://fohm.maps.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data", destfile = temp, mode = "wb", quiet = TRUE)
  swedenResource <- as.data.frame(readxl::read_excel(temp, col_names = TRUE))
  unlink(temp)
  names(swedenResource)[1] <- "date"
  swedenResource$date <- as.Date(swedenResource$date)
  SwedenCounty <- names(swedenResource)[3:length(names(swedenResource))]
  SwedenCounty[SwedenCounty == "J\uE4mtland_H\uE4rjedalen"] <- dplyr::pull(geomSweden[geomSweden$geoid == "SWE752_00_19", ], "micro_name") # Jämtland
  SwedenCounty[SwedenCounty == "S\uF6rmland"] <- dplyr::pull(geomSweden[geomSweden$geoid == "SWE752_00_3", ], "micro_name") # Södermanland
  SwedenCounty[SwedenCounty == "V\uE4stra_G\uF6taland"] <- dplyr::pull(geomSweden[geomSweden$geoid == "SWE752_00_12", ], "micro_name") # Västra Götaland
  names(swedenResource) <- c(names(swedenResource)[1:2], SwedenCounty)

  data <- swedenResource %>%
    tidyr::pivot_longer(3:23, names_to = "County", values_to = "cases") %>%
    dplyr::select(-Totalt_antal_fall) %>%
    dplyr::arrange(dplyr::desc(date))

  # geometry
  # geom <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/sweden-counties.geojson")
  # geom <- st_read("countries/data/geom/geomSweden.geojson")

  # population
  # pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/sweden_pop.csv", encoding = 'UTF-8')
  # pop <- vroom("countries/data/Sweden_pop.csv")


  data_cur <- data %>%
    dplyr::group_by(County) %>%
    dplyr::summarise(County = dplyr::first(County), cases = sum(cases), date = dplyr::first(date))

  data_past <- data %>%
    dplyr::group_by(County) %>%
    dplyr::filter(date <= dplyr::first(lubridate::as_date(date)) - 14) %>%
    dplyr::summarise(County = dplyr::first(County), cases = sum(cases), date = dplyr::first(date))

  data_join <- data_cur %>%
    dplyr::inner_join(data_past, by = "County", suffix = c("", "_past")) %>%
    dplyr::inner_join(pop_sweden, by = c("County")) %>%
    dplyr::mutate(
      Difference = (cases - cases_past) * 10 / as.numeric(date - date_past)
    )


  # integrate datasets
  SwedenMap <- dplyr::inner_join(geomSweden, data_join, by = c("micro_name" = "County"))
  SwedenMap$RegionName <- paste(SwedenMap$micro_name, SwedenMap$country_name, sep = ", ")
  SwedenMap$Country <- SwedenMap$country_name
  SwedenMap$DateReport <- as.character(SwedenMap$date)
  SwedenMap$pInf <- SwedenMap$Difference / SwedenMap$Population
  SWEDEN_DATA <- subset(SwedenMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(SWEDEN_DATA)
}
