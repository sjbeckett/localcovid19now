#' LoadMexico
#'
#' @description Reads in subnational data for Mexico to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' COVID-19 data is sourced from the Covid-19 México hub page: \url{https://datos.covid-19.conacyt.mx/}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Mexico <- LoadMexico()
#' }
#' @seealso [LoadData()]
#' @export
LoadMexico <- function() {
  # COVID-19 Covid-19 México hub page: https://datos.covid-19.conacyt.mx/

  # Main COVID-19 hub page: https://datos.covid-19.conacyt.mx/#DownZCSV
  # need to try 2 days if it doesn't work.

  geomMexico <- NULL
  utils::data("geomMexico", envir = environment())
  geomMexico <- sf::st_as_sf(geomMexico)

  flag <- 0
  aa <- 1
  MEX <- NULL
  
  while (flag == 0) {
    DATE <- Sys.Date() - aa
    formDATE <- format(DATE, "%Y%m%d")
    STRING <- paste0("https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Municipio_Confirmados_", formDATE)
    MEX <- try(vroom::vroom(STRING, show_col_types = FALSE, progress = FALSE), silent = TRUE) # note older files are DELETED.
    if (is.null(MEX) == FALSE) {
      flag <- 1
    } else {
      aa <- aa + 1
    }
    if (aa > 30) {
      stop("no recent data")
      flag <- 2
    }
  }

  # NUMCHARS = nchar(as.character(MEX$cve_ent))
  # IND = which(NUMCHARS==4)

  DataJoin <- c()
  SZ <- dim(MEX)
  DataJoin$Name <- MEX$nombre
  NAME <- strsplit(names(MEX)[SZ[2]], "X")
  DataJoin$pop <- MEX$poblacion
  DataJoin$DateReport <- lubridate::dmy(NAME) # as.Date(NAME[[1]][2],"%d.%m.%Y")
  DataJoin$CaseDiff <- rowSums(MEX[, (SZ[2] - 14):(SZ[2])]) / 14 * 10
  DataJoin$pInf <- DataJoin$CaseDiff / DataJoin$pop
  DataJoin$CVE <- stringr::str_pad(as.character(MEX$cve_ent), pad = "0", side = "left", width = 5)

  DATA <- as.data.frame(DataJoin)

  # geography
  # geomMEX = st_read("countries/data/geom/geomMexico.geojson")
  # for joins see map on Mexican health website: https://datos.covid-19.conacyt.mx/fHDMap/mun.php
  # CH = st_union( geomMEX[c(1815,1821),] ) %>% st_cast("MULTIPOLYGON")
  # geomMEX$geometry[1815]  = CH
  # CH = st_union( geomMEX[c(172,203),] ) %>% st_cast("MULTIPOLYGON")
  # geomMEX$geometry[172]  = CH
  # CH = st_union( geomMEX[c(160,199),] ) %>% st_cast("MULTIPOLYGON")
  # geomMEX$geometry[160]  = CH
  # CH = st_union( geomMEX[c(200,152),] ) %>% st_cast("MULTIPOLYGON")
  # geomMEX$geometry[152]  = CH
  # CH = st_union( geomMEX[c(201,186),] ) %>% st_cast("MULTIPOLYGON")
  # geomMEX$geometry[186]  = CH
  # CH = st_union( geomMEX[c(202,107),] ) %>% st_cast("MULTIPOLYGON")
  # geomMEX$geometry[107]  = CH
  # CH = st_union( geomMEX[c(933,914),] ) %>% st_cast("MULTIPOLYGON")
  # geomMEX$geometry[914]  = CH
  # CH = st_union( geomMEX[c(934,916),] ) %>% st_cast("MULTIPOLYGON")
  # geomMEX$geometry[916]  = CH
  # geomMEX = geomMEX[-c(1821,203,199,200,201,202,933,934),]
  # geomMEX = geomMEX[c("CVEGEO","NOMGEO","estado","geometry")]
  # st_write(geomMEX,"countries/data/geom/geomMexico.geojson")
  # geomMEX = st_read("countries/data/geom/geomMexico.geojson")
  # data("geomMexico")
  # integrate datasets
  MexicoMap <- dplyr::inner_join(geomMexico, DATA, by = c("micro_code" = "CVE"))

  MexicoMap$RegionName <- paste(MexicoMap$micro_name, MexicoMap$macro_name, MexicoMap$country_name, sep = ", ")

  MexicoMap$Country <- MexicoMap$country_name
  MexicoMap$DateReport <- as.character(MexicoMap$DateReport)
  MEXICO_DATA <- subset(MexicoMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(MEXICO_DATA)
}
