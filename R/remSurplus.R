#' Remove Surplus Geometries
#'
#' @param input_file In file
#' @param collection_files File(s) with geoms to remove
#'
#' @return input_file Updated input file
#' @export
#'
#' @examples
#' \dontrun{
#' data("geomGlobal")
#' remSurplus(geomGlobal)
#' }
remSurplus <- function(input_file,
                       collection_files = c("geomEurope", "geomSmallCountries")) {
  isoFile <- input_file %>%
    sf::st_drop_geometry() %>%
    dplyr::select(iso3, filename) %>%
    dplyr::distinct()

  coll1Iso <- isoFile %>%
    dplyr::filter(filename == collection_files[1]) %>%
    dplyr::pull(iso3)
  distIso <- isoFile %>%
    dplyr::filter(!filename %in% collection_files) %>%
    dplyr::pull(iso3)

  input_file <- input_file %>%
    dplyr::mutate(
      isDup = dplyr::case_when(
        filename == collection_files[2] & iso3 %in% coll1Iso ~ 1,
        filename %in% collection_files & iso3 %in% c(distIso, "ASM", "GUM", "MNP", "VIR", "PRI") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::filter(isDup == 0, !is.na(iso3)) %>%
    dplyr::select(-isDup)

  return(input_file)
}
