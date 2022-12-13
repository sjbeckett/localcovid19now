#' LoadPhilippines
#'
#' @description Reads in subnational data for Philippines to calculate most recent estimate of per capita active COVID-19 cases. Note that this process requires the use of the googledrive package which requires the users google credentials to be provided. Use with LoadData() is recommended.
#'
#' @note
#' Data obtained from the Republic of Philippines Department of Health: \url{https://doh.gov.ph/covid19tracker}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Philippines <- LoadPhilippines()
#' }
#' @seealso [LoadData()]
#' @export
LoadPhilippines <- function() {
  . <- geomPhilippines <- pop_philippines <- name <- id <- ProvRes <- DateRepConf <- RegionRes <- TotalReported <- Province <- pop_philippines <- NULL
  utils::data("geomPhilippines", envir = environment())
  utils::data("pop_philippines", envir = environment())
  geomPhilippines <- sf::st_as_sf(geomPhilippines)

  # Republic of Philippines Department of Health: https://doh.gov.ph/covid19tracker

  rlang::check_installed(c("pdftools", "googledrive"), reason = "to use `LoadPhilippines()`")
  
  check_gdrive_scope()

  # silence googledrive in this specific scope
  googledrive::local_drive_quiet()

  url1 <- "bit.ly/DataDropPH"
  req1 <- httr::GET(url1)
  folder_dr <- googledrive::drive_ls(stringr::str_extract(req1$url, "[:graph:]*(?=\\?)"))

  data_link <- googledrive::drive_read_raw(
    file = paste("https://drive.google.com/file/d", dplyr::pull(folder_dr["id"]), sep = "/")
  ) %>%
    pdftools::pdf_text() %>%
    stringr::str_extract("(?<=bit.ly)[:graph:]*") %>%
    .[!is.na(.)]

  url2 <- paste0("https://bit.ly", data_link)

  req2 <- httr::GET(url2)
  folder_data <- googledrive::drive_ls(stringr::str_extract(req2$url, "[:graph:]*(?=\\?)"))

  caseinfo_ids <- folder_data %>%
    dplyr::filter(stringr::str_detect(name, "04 Case Information")) %>%
    dplyr::arrange(name) %>%
    dplyr::select(id) %>%
    dplyr::pull()

  purrr::map_df(
    caseinfo_ids,
    function(x) {
      googledrive::local_drive_quiet()
      temp <- tempfile()
      googledrive::drive_download(
        file = paste("https://drive.google.com/file/d", x, sep = "/"),
        path = temp
      )
      A <- vroom::vroom(
        temp,
        col_types = vroom::cols(
          DateSpecimen = vroom::col_date(format = "%Y-%m-%d"),
          DateResultRelease = vroom::col_date(format = "%Y-%m-%d"),
          DateRepConf = vroom::col_date(format = "%Y-%m-%d"),
          DateDied = vroom::col_date(format = "%Y-%m-%d"),
          DateRecover = vroom::col_date(format = "%Y-%m-%d"),
          DateOnset = vroom::col_date(format = "%Y-%m-%d")
        ),
        show_col_types = FALSE, progress = FALSE
      )
      unlink(temp)
      return(A)
    }
  ) -> case_details

  philippinesData <- case_details %>%
    dplyr::mutate(
      ProvRes = dplyr::case_when(
        stringr::str_detect(ProvRes, "\\(") == T ~ stringr::str_to_title(stringr::str_replace(ProvRes, "\\s\\(([:graph:]*[:blank:]?)*\\)", "")),
        RegionRes == "NCR" & is.na(ProvRes) ~ "NCR", # the NCR region doesn't have provinces, so I would assume that if the Region is NCR, then the province would also be NCR.
        TRUE ~ stringr::str_to_title(ProvRes)
      ),
      ProvRes = stringr::str_replace_all(
        ProvRes,
        "\\sDel\\s",
        " del "
      ),
      ProvRes = stringr::str_replace_all(
        ProvRes,
        "\\sDe\\s",
        " de "
      ),
      ProvRes = stringr::str_replace_all(
        ProvRes,
        "\\sOf\\s",
        " of "
      ),
      ProvRes = stringr::str_replace_all(
        ProvRes,
        "^Ncr$",
        "NCR"
      )
    ) %>%
    dplyr::group_by(
      DateRepConf,
      RegionRes,
      ProvRes
    ) %>%
    dplyr::summarise(
      TotalReported = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      Date = DateRepConf,
      Region = RegionRes,
      Province = ProvRes,
      Cases = TotalReported
    )

  ### Population
  # data("pop_philippines")

  ### Municipalities:
  province <- unique(philippinesData$Province)
  province <- province[is.na(province) == FALSE] # remove NA values

  getDataPH <- function(pro_codes) {
    Table <- c()
    for (aa in 1:length(pro_codes)) {
      code <- pro_codes[aa]
      temp <- philippinesData %>% dplyr::filter(Province == code)
      temp$CumSum <- cumsum(temp$Cases)
      today <- temp$Date[length(temp$Date)]
      past_date <- today - 14
      pastData <- temp[temp$Date <= past_date, ]
      difference <- (temp$CumSum[length(temp$CumSum)] - pastData$CumSum[length(pastData$CumSum)]) / 14 * 10
      vec <- data.frame(Province = code, Date = today, Difference = difference)
      Table <- rbind(Table, vec)
    }
    return(Table)
  }

  philippinesTable <- getDataPH(province)

  ### Geometry:
  # data("geomPhilippines")

  geomPhilippines <- geomPhilippines %>%
    dplyr::left_join(
      pop_philippines,
      by = c("micro_name" = "Location")
    )

  philippinesMap <- dplyr::inner_join(geomPhilippines, philippinesTable, by = c("micro_name" = "Province"))
  philippinesMap$RegionName <- paste(philippinesMap$micro_name, philippinesMap$country_name, sep = ", ")
  philippinesMap$Country <- philippinesMap$country_name
  philippinesMap$DateReport <- as.character(philippinesMap$Date)
  philippinesMap$pInf <- philippinesMap$Difference / philippinesMap$Pop2015
  PHILIPPINES_DATA <- subset(philippinesMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(PHILIPPINES_DATA)
}


#' check_gdrive_scope
#' @description runs a googledrive function that requires permissions to be shared by user with the tidyverse API. This function is designed to check if these permissions (needed for LoadPhilippines to work) are enabled, and error if not.
#' @keywords internal
#' @examples 
#' \dontrun{
#' check_gdrive_scope()
#' }
#' @export
check_gdrive_scope <- function(){
  check <- NULL
  tryCatch({
    check <- googledrive::drive_about()
  },error = function(e){
    stop("Insufficient permissions provided to googledrive.\nData requiring googledrive will not be loaded. \nTo provide permissions use googledrive::drive_auth().")
  })
}