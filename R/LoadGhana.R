#' LoadGhana
#'
#' @description Reads in subnational data for Ghana to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data provided on HDX by Safeture from the Ghana Health Service (Ministry of Health): \url{https://data.humdata.org/dataset/ghana-coronavirus-covid-19-subnational-cases}.
#' Geometry collected by Where Geospatial Media (\url{www.wheregeospatial.com} \url{blog.wheregeospatial.com}). This Data is not authorized by any government or private institution. It was created from the old authorized Ghana Shapefile using the district boundaries. Any one who uses this for any analysis should note this. Any limitations from the old authorized shapefile are inherited by this shapefile. Created Dec 11, 2019.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Ghana <- LoadGhana()
#' @seealso [LoadData()]
#' @export
LoadGhana <- function() {
  name <- cases <- latest_date <- past_date <- cumulative_cases <- macro_name <- country_name <- report_date <- case_diff <- DateReport <- geoid <- RegionName <- pInf <- NULL

  # Load in geometry
  utils::data("geomGhana", envir = environment())
  geomGhana <- sf::st_as_sf(geomGhana)

  temp <- tempfile()
  utils::download.file(url = "https://www.dropbox.com/s/2uxzix4upet0nlm/cases_ghana.csv?dl=1", destfile = temp, quiet = TRUE)
  casesGhana <- vroom::vroom(temp, show_col_types = FALSE, progress = FALSE)

  ghanaCases <- casesGhana %>%
    dplyr::group_by(name) %>%
    dplyr::select(-dplyr::starts_with("iso"), -cases) %>%
    dplyr::mutate(
      latest_date = max(date)
    ) %>%
    dplyr::filter(
      date <= (latest_date - 14) | date == latest_date
    ) %>%
    dplyr::mutate(
      past_date = max(date[date != latest_date])
    ) %>%
    dplyr::filter(
      date %in% c(latest_date, past_date)
    ) %>%
    dplyr::summarise(
      report_date = max(date),
      case_diff = max(cumulative_cases) - min(cumulative_cases) * 10 / as.numeric(dplyr::first(latest_date) - dplyr::first(past_date))
    ) %>%
    tidyr::separate(
      col = name,
      into = c("name", "geography"),
      sep = "\\s(?=Region)"
    ) %>%
    dplyr::mutate(
      name = dplyr::case_when(
        name == "Brong-Ahafo" ~ "Ahafo",
        TRUE ~ name
      )
    )

  pop <- dplyr::tibble(
    name = c("Ahafo", "Ashanti", "Bono", "Bono East", "Central", "Eastern", "Greater Accra", "North East", "Northern", "Oti", "Savannah", "Upper East", "Upper West", "Volta", "Western", "Western North"),
    pop = c(564668, 5440463, 1208649, 653266, 2859821, 2925653, 5455692, 658946, 2310939, 747248, 1203400, 1301226, 901502, 1659040, 2060585, 880921)
  )

  GHANADATA <- geomGhana %>%
    dplyr::inner_join(ghanaCases, by = c("macro_name" = "name")) %>%
    dplyr::inner_join(pop, by = c("macro_name" = "name")) %>%
    dplyr::mutate(
      RegionName = paste(macro_name, country_name, sep = ", "),
      DateReport = as.character(report_date),
      pInf = case_diff / pop
    ) %>%
    dplyr::select(
      DateReport, geoid, RegionName,
      Country = country_name, pInf
    )

  return(GHANADATA)
}
