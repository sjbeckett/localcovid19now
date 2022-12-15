#' Update Global Geometries
#'
#' @description Function to integrate in new geometries and update the geometries across the package
#'
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' localcovid19now:::updateGlobal()
#' }
updateGlobal <- function() {
  filename <- geoid <- m49code <- iso3 <- country_name <- macro_code <- macro_name <- micro_code <- micro_name <- NULL

  # Requires rmapshaper for the ms_simplify function - simplifies polygons.
  rlang::check_installed(c("rmapshaper", "here"), reason = "to use `updateGlobal()`")

  # Open the most recent WorldPreSimplified geojson. Manually modify if this is not the most up-to-date file.
  ## Because zipfile named with minutes post epoch, `max(dir("updateGeometry/WorldPreSimplified"))` gives most recent version. Using ctime property potentially not reliable bc file creation with git.
  zipfiles <- list.files(here::here("tools/WorldPreSimplified"))

  temp <- utils::unzip(zipfile = here::here("tools/WorldPreSimplified", max(zipfiles)), exdir = tempdir())

  geomWorld <- sf::st_read(temp)
  unlink(temp)

  ### Instructions
  ## Add original geomCountry.geojson to countries/data/orig_geom
  ## Create new script "updateGeometry/toProcess/processCounty.R" that matches processTemplate's format.
  ## Run updateGlobal.R

  # source("updateGeometry/addNewGeom.R")
  newWorld <- addNewGeoms()

  # If new geometries are added in newWorld, remove the old ones from geomWorld and add the new ones
  if (!is.null(newWorld)) { # If no new geometries are added, newWorld will be NULL
    updatedFiles <- unique(newWorld$filename)
    geomWorld <- geomWorld %>%
      dplyr::filter(!filename %in% updatedFiles) %>%
      dplyr::bind_rows(
        newWorld
      )

    # Remove geomSmallCountries and geomEurope entries that are supplied by other data sets
    # source("updateGeometry/remSurplus.R")
    geomWorld <- remGeoSurplus(geomWorld, c("geomEurope", "geomSmallCountries"))

    # Write a new geomWorld_presimp that includes the updated geometry
    sf::st_write(geomWorld, "geomWorld_presimp.geojson", delete_dsn = T)

    nowtime <- round(difftime(lubridate::now(tzone = "UTC"), lubridate::ymd_hms("1970-01-01 00:00:00"), tz = "UTC", units = "mins"))
    # the presimplified world geometry is too big for github, so put it in a zip file and remove the geojson
    zip(zipfile = paste0("tools/WorldPreSimplified/WorldPreSimp", nowtime, ".zip"), "geomWorld_presimp.geojson")
    file.remove("geomWorld_presimp.geojson")
    rm(newWorld)
  } else {
    updatedFiles <- NULL
  }

  # Simplify the world
  geomGlobal <- rmapshaper::ms_simplify(geomWorld, keep = 0.05, explode = T, keep_shapes = TRUE) %>%
    dplyr::group_by(geoid) %>%
    sf::st_make_valid() %>%
    # There are some duplicated geoids due to geomSmallCountries, but these are filtered out in the LoadCountry stage
    dplyr::summarise(
      m49code = dplyr::first(m49code),
      iso3 = dplyr::first(iso3),
      country_name = dplyr::first(country_name),
      macro_code = dplyr::first(macro_code),
      macro_name = dplyr::first(macro_name),
      micro_code = dplyr::first(micro_code),
      micro_name = dplyr::first(micro_name),
      filename = dplyr::first(filename)
    ) %>%
    dplyr::ungroup() %>%
    sf::st_wrap_dateline() %>% # removes horizontal bar when Fiji crosses the dateline
    sf::st_collection_extract() %>%
    sf::st_cast("MULTIPOLYGON")

  # ## Add mapshapper clean code here

  # Saving entire clean geometry, not sure which method we want to use. It currently isn't called anywhere in the package, but it could be later if we wanted to only join to geometry once.
  # sf::st_write(geomGlobal, "tools/geomGlobal_simplified.geojson", delete_dsn = T)
  usethis::use_data(geomGlobal, overwrite = T)

  # group geomGlobal by filename
  groupedGlobal <- geomGlobal %>%
    dplyr::group_by(filename)
  # create a list of each group
  globalList <- dplyr::group_split(groupedGlobal, .keep = F) # remove grouping property
  # name each stage of the list with the filename (group_key = filename)
  names(globalList) <- dplyr::pull(dplyr::group_keys(groupedGlobal))

  # Save each geometry as filename.Rda for the package to use
  for (y in seq_along(globalList)) {
    try({
      cat("\n")
      # if (file.exists(here::here("data", paste0(names(globalList[y]), ".rda"))) & !names(globalList[y]) %in% updatedFiles) {

      # If the relevant geometry is already in the pacakge, check if it's different than the new one
      if (exists(names(globalList[y])) & !names(globalList[y]) %in% updatedFiles) {
        # Bring in existing geometry
        data(list = names(globalList[y]), envir = environment())
        assign(x = "existing", get(names(globalList[y])))

        # existing <- sf::st_read(paste0("countries/data/geom/", names(globalList[y]), ".geojson"), quiet = T)
        # Isolate the new geometry
        newgeom <- globalList[[y]]

        # If all geometry of the new and existing files AND all the dataframe values of new and existing are the same, do not overwrite the existing file
        if (nrow(newgeom) != nrow(existing)) {
          # If they aren't the same, overwrite the existing file
          message("overwrite existing geometry: ", names(globalList[y]), "\n")
          # assign(x = names(globalList[y]), value = globalList[y][[1]])
          # usethis::use_data(get(names(globalList[y])), overwrite = T) # Use data for package
          # sf::st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/", names(globalList[y]), ".geojson"), delete_dsn = T)

          purrr::walk2(globalList[y], names(globalList[y]), function(obj, name) {
            assign(name, obj)
            do.call(eval(parse(text = "usethis::use_data")), list(as.name(name), overwrite = TRUE))
          })
        } else if (!all(purrr::map_lgl(
          1:nrow(newgeom),
          ~ sf::st_geometry(existing)[.x] == sf::st_geometry(newgeom)[.x]
        ), na.rm = T) & !all(sf::st_drop_geometry(existing) == sf::st_drop_geometry(newgeom), na.rm = T)
        ) {
          # If they aren't the same, overwrite the existing file
          message("overwrite existing geometry: ", names(globalList[y]), "\n")
          # assign(x = names(globalList[y]), value = globalList[y][[1]])
          # usethis::use_data(get(names(globalList[y])), overwrite = T) # Use data for package
          # sf::st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/", names(globalList[y]), ".geojson"), delete_dsn = T)

          purrr::walk2(globalList[y], names(globalList[y]), function(obj, name) {
            assign(name, obj)
            do.call(eval(parse(text = "usethis::use_data")), list(as.name(name), overwrite = TRUE))
          })

          rm(list = names(globalList[y]))
        } else {
          cat("geometries (", y, ") are the same\n")
        }
      } else {
        # If the geometry is part of the newly updated geometries (updatedFiles), over(write) without checking as changes have been made and it may not be the same size as the existing geometry, which would throw an error in the similarity check
        message("write new geometry: ", names(globalList[y]), "\n")
        # assign(x = names(globalList[y]), value = globalList[y][[1]])
        # usethis::use_data(get(names(globalList[y])), overwrite = T) # Use data for package
        # sf::st_write(obj = get(names(globalList[y])), paste0("countries/data/geom/", names(globalList[y]), ".geojson"), delete_dsn = T)
        purrr::walk2(globalList[y], names(globalList[y]), function(obj, name) {
          assign(name, obj)
          do.call(eval(parse(text = "usethis::use_data")), list(as.name(name), overwrite = TRUE))
        })
        rm(list = names(globalList[y]))
      }
    })
  }
  rm(existing, newgeom)
}


#' Move Processed Files
#'
#' @description Moves files from "toProcess" to "processed" folder when updating country geometries. Designed to only work with the "processFileName" naming convention. This is part of the geometry updating system and is designed for developers.
#' @param x File name to be moved
#' @param ... pass arguments to `file.copy`
#' @keywords internal
#'
#'
#'
#' @examples
#' \dontrun{
#' dir.create(here::here("tools", "toProcess"))
#' file.create(here::here("tools", "toProcess", "processTestFile.R"))
#' localcovid19now:::moveProcessedFiles("processTestFile.R")
#' }
moveProcessedFiles <- function(x, ...) {
  rlang::check_installed(c("here"), reason = "to use moveProcessedFiles()")
  if (stringr::str_sub(x, 1, 7) != "process") {
    stop("File", x, "is not formatted correctly. Please review standards.")
  } else {
    cat("\n", x, "...")
    if (
      file.copy(
        from = here::here("tools", "toProcess", x),
        to = here::here("tools", "processed", x), ...
      )) {
      cat(" copied ...")
      if (
        file.remove(here::here("tools", "toProcess", x))) {
        cat(" moved ...\n")
      } else {
        stop("failed to delete", paste0("tools/toProcess/", x))
      }
    } else {
      cat("\n")
      stop("failed to copy ", paste0("tools/toProcess/", x))
    }
  }
}


#' Add New Country Geometries
#'
#' @description Internal function for the geometry update process. Processes new geometries using "processX.R" scripts in the "tools/toProcess" folder and further formats the geometries correctly to be integrated into the system overall. Running this function outside of a geometry update may cause errors from moving process scripts prematurely.
#' @return world Map with new geoms added
#' @keywords internal
#'
#'
#' @examples
#' \dontrun{
#' localcovid19now:::addNewGeoms()
#' }
addNewGeoms <- function() {
  iso3 <- m49code <- macro_code <- micro_code <- geoid <- country_name <- macro_name <- NULL

  rlang::check_installed(c("here"), reason = "to use addNewGeoms()")
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

  file_list <- list.files(here::here("tools", "toProcess"))[which(stringr::str_sub(list.files(here::here("tools", "toProcess")), 1, 7) == "process")] # only use files formatted as "processX"

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

    lapply(file_list, moveProcessedFiles)


    return(worldNew)
  } else {
    message("\nNo new geometries processed. toProcess folder is empty.\n")
  }
}

#' Reset processed files
#'
#'  @description Resets all files in the "tools/process" folder back to "tools/toProcess". Useful in the event that something was processed improperly and you need to reset the geometries to a more stable version. Running when not necessary will result in files being moved to the toProcess folder that do not need to be processed again. This could lead to errors if there are conflicting files in the "processed" folder.
#'  @param file_list a vector of file names to be reset. if `NULL`, default, all files in the `processed` folder will be reset.
#' @param ... pass arguments to `file.copy`
#' @keywords internal
#'
#'
#' @examples
#' \dontrun{
#' dir.create(here::here("tools", "toProcess"))
#' file.create(here::here("tools", "processed", "processTestFile.R"))
#' localcovid19now:::resetNewGeoms("processTestFile.R")
#' }
resetNewGeoms <- function(file_list = NULL, ...) {
  rlang::check_installed(c("here"), reason = "to use resetNewGeoms()")

  if (is.null(file_list)) {
    file_list <- list.files(here::here("tools", "processed"))[which(stringr::str_sub(list.files(here::here("tools", "processed")), 1, 7) == "process")]
    message("\nresetting all processed geometry scripts...\n")
  }
  lapply(
    file_list,
    \(x){
      cat("\n", x, "...")
      if (
        file.copy(
          from = here::here("tools", "processed", x),
          to = here::here("tools", "toProcess", x), ...
        )) {
        cat(" copied ...")
        if (
          file.remove(here::here("tools", "processed", x))) {
          cat(" moved ...\n")
        } else {
          stop("failed to delete", paste0("tools/toProcess/", x))
        }
      } else {
        stop("failed to copy", paste0("tools/toProcess/", x))
      }
    }
  )
}



#' Remove Surplus Geometries
#'
#' @description Some geography files are collections of many countries that overlap with other files that have their own higher-quality data. The remGeoSurplus function ensures each country is only present once in the dataset.
#'
#' @param input_file In file
#' @param collection_files File(s) with geoms to remove
#'
#' @return input_file Updated input file
#'
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' data("geomGlobal")
#' localcovid19now:::remGeoSurplus(geomGlobal)
#' }
remGeoSurplus <- function(input_file,
                          collection_files = c("geomEurope", "geomSmallCountries")) {
  iso3 <- filename <- isDup <- NULL
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
