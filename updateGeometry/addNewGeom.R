here::i_am("updateGeometry/addNewGeom.R")

moveFiles <- function(x) {
  file.copy(
    from = here::here("updateGeometry", "toProcess", x),
    to = here::here("updateGeometry", "processed", x)
  )
  file.remove(here::here("updateGeometry", "toProcess", x))
}

# Add new countries
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

  necessary_cols <- tibble("macro_code" = NA_character_, "macro_name" = NA_character_, "micro_code" = NA_character_, "micro_name" = NA_character_)

  ## Load in the m49 codes for countries
  m49 <- readxl::read_xlsx(here::here("updateGeometry/UNSD_m49.xlsx"))
  m49 <<- m49 %>%
    rename_with(
      .fn = \(x) str_replace_all(x, "[\\s/-]", "")
    )

  file_list <- list.files(here::here("updateGeometry", "toProcess"))

  if (length(file_list) > 0) {
    lapply(file_list, \(x) source(here::here("updateGeometry", "toProcess", x)))
    geom_list <- paste0("geom", str_extract(file_list, "(?<=process)[:alpha:]+(?=.R$)"))

    worldNew <- purrr::map_df(
      geom_list,
      ~ get(.x) %>%
        mutate(
          filename = .x
        ) %>%
        st_collection_extract() %>%
        st_cast("MULTIPOLYGON")
    ) %>%
      left_join(necessary_cols) %>%
      replace_na(list("macro_code" = "00", "micro_code" = "00")) %>%
      mutate(
        geoid = paste(paste0(iso3, m49code), macro_code, micro_code, sep = "_")
      ) %>%
      select(geoid, m49code, iso3, country_name, macro_code, macro_name, everything()) %>%
      st_as_sf() %>%
      tibble::remove_rownames()

    lapply(file_list, moveFiles)


    return(worldNew)
  }
}

resetNewGeoms <- function() {
  file_list <- list.files(here::here("updateGeometry", "processed"))

  lapply(
    file_list,
    \(x){
      file.copy(
        from = here::here("updateGeometry", "processed", x),
        to = here::here("updateGeometry", "toProcess", x)
      )
      file.remove(here::here("updateGeometry", "processed", x))
    }
  )
}
