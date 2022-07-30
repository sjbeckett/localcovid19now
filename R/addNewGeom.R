#' Move Processed Files
#'
#' @param x File name to be moved
#'
#' @export
#'
#' @examples
#' file.create(here::here("tools", "toProcess", "testFile.R"))
#' moveFiles("testFile.R")
moveFiles <- function(x) {
  file.copy(
    from = here::here("tools", "toProcess", x),
    to = here::here("tools", "processed", x)
  )
  file.remove(here::here("tools", "toProcess", x))
}

#' Add New Country Geometries
#'
#' @return world Map with new geoms added
#'
#' @export
#'
#' @examples
#' \dontrun{
#' addNewGeoms()
#' }
addNewGeoms <- function() {
  # Fields will be as follows:
  ## geoid = ISO3m49_micro_macro
  ## m49code
  ## iso3
  ## county_name
  ## macro_code
  ## macro_name
  ## micro_code
  ## micro_name
  ## geometry

  necessary_cols <- dplyr::tibble("macro_code" = NA_character_, "macro_name" = NA_character_, "micro_code" = NA_character_, "micro_name" = NA_character_)

  ## Load in the m49 codes for countries
  utils::data("m49", envir = environment())

  file_list <- list.files(here::here("tools", "toProcess"))

  if (length(file_list) > 0) {
    # This "source" needs to be replaced bc it's bad form in a package, but I haven't figured out how it would work better yet
    lapply(file_list, \(x) source(here::here("tools", "toProcess", x)))
    geom_list <- paste0("geom", stringr::str_extract(file_list, "(?<=process)[:alpha:]+(?=.R$)"))

    worldNew <- purrr::map_df(
      geom_list,
      ~ get(.x) %>%
        dplyr::mutate(
          filename = .x
        ) %>%
        sf::st_collection_extract() %>%
        sf::st_cast("MULTIPOLYGON")
    ) %>%
      dplyr::left_join(necessary_cols) %>%
      tidyr::replace_na(list("macro_code" = "00", "micro_code" = "00")) %>%
      dplyr::mutate(
        geoid = paste(paste0(iso3, m49code), macro_code, micro_code, sep = "_")
      ) %>%
      dplyr::select(geoid, m49code, iso3, country_name, macro_code, macro_name, dplyr::everything()) %>%
      sf::st_as_sf() %>%
      tibble::remove_rownames()

    lapply(file_list, moveFiles)


    return(worldNew)
  }
}

#' Reset processed files
#'
#' @export
#'
#' @examples
#' \dontrun{
#' resetNewGeoms()
#' }
resetNewGeoms <- function() {
  file_list <- list.files(here::here("tools", "processed"))

  lapply(
    file_list,
    \(x){
      file.copy(
        from = here::here("tools", "processed", x),
        to = here::here("tools", "toProcess", x)
      )
      file.remove(here::here("tools", "processed", x))
    }
  )
}
