# Add new countries
addNewGeoms <- function() {
  # Fields will be as follows:
  ## GEOID = ISO3m49_micro_macro
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
  m49 <- readxl::read_xlsx("UNSD_m49.xlsx")
  m49 <- m49 %>%
    rename_with(
      .fn = \(x) str_replace_all(x, "[\\s/-]", "")
    )

  file_list <- list.files("updateGeometry/toProcess", full.names = T)

  if (length(file_list) > 0) {
    lapply(file_list, source)
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
      # tibble::add_column(., !!!necessary_cols[!names(necessary_cols) %in% names(.)])%>%
      replace_na(list("macro_code" = "00", "micro_code" = "00")) %>%
      mutate(
        geoid = paste(paste0(iso3, m49code), macro_code, micro_code, sep = "_")
      ) %>%
      select(geoid, m49code, iso3, country_name, macro_code, macro_name, everything()) %>%
      st_as_sf() %>%
      tibble::remove_rownames()

    moveFiles <- function(x) {
      file.copy(
        from = x,
        to = str_replace(x, "toProcess", "processed")
      )
      file.remove(x)
    }

    lapply(file_list, moveFiles)


    return(worldNew)
  }
}

resetNewGeoms <- function() {
  file_list <- list.files("updateGeometry/processed", full.names = T)

  lapply(
    file_list,
    \(x){
      file.copy(
        from = x,
        to = str_replace(x, "processed", "toProcess")
      )
      file.remove(x)
    }
  )
}
