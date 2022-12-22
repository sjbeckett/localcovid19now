#' LoadEurope
#'
#' @description Reads in subnational data for Europe to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data aggregated from local health resources in the WHO European Region COVID19 Subnational Explorer \url{https://experience.arcgis.com/experience/3a056fc8839d47969ef59949e9984a71}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Europe <- LoadEurope()
#' @seealso [LoadData()]
#' @export
LoadEurope <- function() {
  # Data aggregated from local health resources in the WHO European Region COVID19 Subnational Explorer https://experience.arcgis.com/experience/3a056fc8839d47969ef59949e9984a71
  # https://www.arcgis.com/home/item.html?id=494604e767074ce1946d86aa4d8a3b5a

  geomEurope <- NULL
  utils::data("geomEurope", envir = environment())
  geomEurope <- sf::st_as_sf(geomEurope)
  
  # most uptodate data
  # EUWHO = read.csv("https://arcgis.com/sharing/rest/content/items/54d73d4fd4d94a0c8a9651bc4cd59be0/data",encoding="UTF-8")

  
  EUWHO <- vroom::vroom("https://arcgis.com/sharing/rest/content/items/54d73d4fd4d94a0c8a9651bc4cd59be0/data", col_types = c(DateRpt = "c"), show_col_types = FALSE, progress = FALSE)

  EUWHO$pInf <- EUWHO$Incidence14day / 100000 * (10 / 14) # incidence is cases in 14 days per 100,000 people. Convert to prop of pop. in 10 days.
  # Join using UID
  EuroMap <- dplyr::inner_join(geomEurope, EUWHO, by = c("micro_code" = "UID"))

  ## Remove countries
  # Turkmenistan - no testing performed. Hence no cases found.
  EuroMap <- EuroMap[-which(EuroMap$country_name == "Turkmenistan"), ]
  # Countries which we have higher resolution maps
  ## This is now performed when creating the geometry files, so we could probably remove it
  CountriesCovered <- c("Italy", "Switzerland", "Ireland", "United Kingdom", "Austria", "France", "Czech Republic", "Spain", "Denmark", "Sweden", "Netherlands", "Germany", "Norway", "Belgium", "Liechtenstein")
  RM_REGIONS <- c()
  for (aa in 1:length(CountriesCovered)) {
    INDXs <- which(EuroMap$country_name == CountriesCovered[aa])
    # if(CountriesCovered[aa]=="United Kingdom"){ #keep UK crown dependencies.
    # 		INDXs = INDXs[-which(EuroMap$Region[INDXs]=="Isle of Man")]
    # 		INDXs = INDXs[-which(EuroMap$Region[INDXs]=="Guernsey")]
    # 		INDXs = INDXs[-which(EuroMap$Region[INDXs]=="Jersey")]
    # 		INDXs = INDXs[-which(EuroMap$Region[INDXs]=="Gibraltar")]
    # }
    # if(CountriesCovered[aa]=="Denmark"){ #keep Faroe Islands, Greenland
    # 		INDXs = INDXs[-which(EuroMap$Region[INDXs]=="Faeroe Islands")]
    # 		INDXs = INDXs[-which(EuroMap$Region[INDXs]=="Greenland")]
    # }
    if (CountriesCovered[aa] == "Norway") { # keep Svalbard and Jan Mayen Islands
      INDXs <- INDXs[-which(EuroMap$micro_name[INDXs] == "Svalbard and Jan Mayen Islands")]
    }
    RM_REGIONS <- c(RM_REGIONS, INDXs)
  }
  if (length(RM_REGIONS) > 0) {
    EuroMap <- EuroMap[-RM_REGIONS, ]
  }
  EuroMap <- EuroMap %>%
    dplyr::mutate(
      country_name = dplyr::case_when(
        micro_name %in% c("Isle of Man", "Guernsey", "Jersey", "Gibraltar") ~ "United Kingdom",
        micro_name %in% c("Faeroe Islands", "Greenland") ~ "Denmark",
        TRUE ~ country_name
      )
    )

  EuroMap$RegionName <- paste(EuroMap$micro_name, EuroMap$country_name, sep = ", ")
  EuroMap$Country <- EuroMap$country_name

  EUROPE_DATA <- subset(EuroMap, select = c("DateRpt", "geoid", "RegionName", "Country", "pInf", "geometry"))
  colnames(EUROPE_DATA)[1] <- "DateReport"

  return(EUROPE_DATA)
}
