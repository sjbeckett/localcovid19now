#' LoadGhana
#'
#' @description Reads in subnational data for Ghana to calculate most recent estimate of per capita active COVID-19 cases.
#'
#' @note
#' Data provided on HDX by Safeture from the Ghana Health Service (Ministry of Health): \url{https://data.humdata.org/dataset/ghana-coronavirus-covid-19-subnational-cases}.
#' Geometry collected by Where Geospatial Media (\url{www.wheregeospatial.com} \url{blog.wheregeospatial.com}). This Data is not authorized by any government or private institution. It was created from the old authorized Ghana Shapefile using the district boundaries. Any one who uses this for any analysis should note this. Any limitations from the old authorized shapefile are inherited by this shapefile. Created Dec 11, 2019.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Ghana <- LoadGhana()
#' }
#' @seealso [LoadCountries()]
#' @export
LoadGhana <- function() {
  data("geomGhana")

  temp <- tempfile()
  download.file(url = "https://www.dropbox.com/s/2uxzix4upet0nlm/cases_ghana.csv?dl=1", destfile = temp)
  casesGhana <- vroom(temp)

  ghanaCases <- casesGhana %>%
    group_by(name) %>%
    select(-starts_with("iso"), -cases) %>%
    mutate(
      latest_date = max(date)
    ) %>%
    filter(
      date <= (latest_date - 14) | date == latest_date
    ) %>%
    mutate(
      past_date = max(date[date != latest_date])
    ) %>%
    filter(
      date %in% c(latest_date, past_date)
    ) %>%
    summarise(
      report_date = max(date),
      case_diff = max(cumulative_cases) - min(cumulative_cases) * 10 / as.numeric(first(latest_date) - first(past_date))
    ) %>%
    separate(
      col = name,
      into = c("name", "geography"),
      sep = "\\s(?=Region)"
    ) %>%
    mutate(
      name = case_when(
        name == "Brong-Ahafo" ~ "Ahafo",
        TRUE ~ name
      )
    )

  pop <- tibble(
    name = c("Ahafo", "Ashanti", "Bono", "Bono East", "Central", "Eastern", "Greater Accra", "North East", "Northern", "Oti", "Savannah", "Upper East", "Upper West", "Volta", "Western", "Western North"),
    pop = c(564668, 5440463, 1208649, 653266, 2859821, 2925653, 5455692, 658946, 2310939, 747248, 1203400, 1301226, 901502, 1659040, 2060585, 880921)
  )

  GHANADATA <- geomGhana %>%
    inner_join(ghanaCases, by = c("macro_name" = "name")) %>%
    inner_join(pop, by = c("macro_name" = "name")) %>%
    mutate(
      RegionName = paste(macro_name, country_name, sep = ", "),
      DateReport = as.character(report_date),
      pInf = case_diff / pop
    ) %>%
    select(
      DateReport, geoid, RegionName,
      Country = country_name, pInf
    )

  return(GHANADATA)
}
