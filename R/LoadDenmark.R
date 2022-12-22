#' LoadDenmark
#'
#' @description Reads in subnational data for Denmark to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#'  COVID-19 data from the Statens Serum Institut (SSI):
#' * \url{https://covid19.ssi.dk/overvagningsdata}
#' * \url{https://experience.arcgis.com/experience/aa41b29149f24e20a4007a0c4e13db1d}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Denmark <- LoadDenmark()
#' }
#' @seealso [LoadData()]
#' @export
LoadDenmark <- function() {
  # COVID-19 data from the Statens Serum Institut (SSI):
  #- https://covid19.ssi.dk/overvagningsdata
  #- https://experience.arcgis.com/experience/aa41b29149f24e20a4007a0c4e13db1d


  SampleDate <- Municipality <- Cases <- CumCases <- .data <- Date <- geomDenmark <- NULL

  # geometry
  # geomDenmark <- st_read('https://raw.githubusercontent.com/magnuslarsen/geoJSON-Danish-municipalities/master/municipalities/municipalities.geojson')
  # #name alteration for matching case data
  # Name2save1 = geomDenmark$micro_name[42] #"Høje-Taastrup"
  # Name2save2 = geomDenmark$micro_name[60] #"Lyngby-Taarbæk"
  # Name2save3 = geomDenmark$micro_name[73] #"Ringkøbing-Skjern"
  #
  utils::data("geomDenmark", envir = environment())
  geomDenmark <- sf::st_as_sf(geomDenmark)

  # case data
  # 1.)  identify file location from webpages
  webpages <- rvest::read_html("https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata")
  # extract the html blocks which are h5 and contain links
  JAM <- webpages %>%
    rvest::html_nodes("h5") %>%
    rvest::html_nodes("a")
  # subset this list by looking for items which are in the directory of interest
  INDEX <- which(grepl("https://files.ssi.dk/covid19/overvagning/data/", JAM, fixed = TRUE))

  # JAM[2] should be the download link -- unless the website changes...Not sure if there is an easy way to double check this is the right code block?

  # Should be able to use """ html_elements(webpages,xpath="/html/body/div[2]/div[2]/section[5]/accordions/div[2]/div/div/ul/li[1]/h5/a")%>%html_attr("href") """ to get the link as well, but I haven't tested it for reliability

  # extract the text from href attribute for the link. this is faster than the substring method
  DOWNLOADLINK <- rvest::html_attr(JAM[INDEX[1]], "href")
  DOWNLOADLINK <- paste0(DOWNLOADLINK, ".zip") # need to add .zip extension in order for the download/extraction process to perform correctly in R.
  # Have the download link!

  # 2.) download and extract data:
  temp <- tempfile() # temporary file for download
  temp2 <- tempdir() # temporary file for extraction
  utils::download.file(DOWNLOADLINK, destfile = temp, quiet = TRUE)
  utils::unzip(zipfile = temp, exdir = temp2)
  DenmarkData <- vroom::vroom(file.path(temp2, "Municipality_cases_time_series.csv"), delim = ";", show_col_types = FALSE, progress = FALSE)
  unlink(temp)
  unlink(temp2)

  curdate <- DenmarkData$SampleDate %>%
    sort() %>%
    dplyr::last()

  dataTable <- DenmarkData %>%
    tidyr::pivot_longer(
      cols = -SampleDate,
      names_to = "Municipality",
      values_to = "Cases"
    ) %>%
    dplyr::group_by(Municipality) %>%
    dplyr::mutate(
      CumCases = cumsum(Cases)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(SampleDate %in% c(curdate, curdate - 14)) %>%
    tidyr::pivot_wider(
      id_cols = Municipality,
      names_from = SampleDate,
      values_from = CumCases,
      names_prefix = "d"
    ) %>%
    dplyr::mutate(
      Difference = (.data[[paste0("d", curdate)]] - .data[[paste0("d", (curdate - 14))]]) * 10 / 14,
      Date = curdate
    ) %>%
    dplyr::select(Date, dplyr::everything(), -dplyr::starts_with("d", ignore.case = F))

  # population
  ## get from Statistics Denmark: https://www.statbank.dk/statbank5a/SelectVarVal/saveselections.asp
  names(pop_denmark) <- c("Municipality", "Population")
  # make the population column as numeric
  pop_denmark$Population <- as.numeric(gsub(" ", "", pop_denmark$Population))

  # integrate datasets
  Denmarkdf <- dplyr::inner_join(dataTable, pop_denmark, by = "Municipality")
  Denmarkdf$Municipality[which(Denmarkdf$Municipality == "Nordfyns")] <- "Nordfyn"
  Denmarkdf$Municipality[which(Denmarkdf$Municipality == "Vesthimmerlands")] <- "Vesthimmerland"

  DenmarkMap <- dplyr::inner_join(geomDenmark, Denmarkdf, by = c("micro_name" = "Municipality"))

  DenmarkMap$RegionName <- paste(DenmarkMap$micro_name, DenmarkMap$country_name, sep = ", ")
  DenmarkMap$Country <- DenmarkMap$country_name
  DenmarkMap$DateReport <- as.character(DenmarkMap$Date)
  DenmarkMap$pInf <- DenmarkMap$Difference / as.numeric(DenmarkMap$Population)
  DENMARK_DATA <- subset(DenmarkMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(DENMARK_DATA)
}
