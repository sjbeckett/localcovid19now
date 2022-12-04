#' LoadIndonesia
#'
#' @description Reads in subnational data for Indonesia to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Live data, interactive charts & maps of Indonesia provincial COVID-19 daily cases and vaccination. Live version: \url{https://erlange.github.io/INACOVID/}
#' \url{https://github.com/erlange/INACOVID}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Indonesia <- LoadIndonesia()
#' @seealso [LoadData()]
#' @export
LoadIndonesia <- function() {
  cases <- vroom::vroom("https://raw.githubusercontent.com/erlange/INACOVID/master/data/csv/ext.prov.csv", show_col_types = FALSE, progress = FALSE) # cases are KASUS. new cases per day.

  micro_name <- NULL
  utils::data("geomIndonesia", envir = environment())
  geomIndonesia <- sf::st_as_sf(geomIndonesia)

  provinces <- unique(cases$Location)
  DateReport <- c()
  CaseDifference <- c()
  for (aa in 1:length(provinces)) {
    subsetcases <- cases[which(cases$Location == provinces[aa]), ]
    DateReport[aa] <- as.character(max(subsetcases$Date))
    recentcases <- subsetcases$KASUS[which(as.Date(subsetcases$Date) > (as.Date(max(subsetcases$Date)) - 14))]
    CaseDifference[aa] <- sum(recentcases) / length(recentcases) * 10
  }

  caseTable <- data.frame(provinces, DateReport, CaseDifference)

  # population
  # from 2020 census. Badan Pusat Statistik/Statistics Indonesia, Jakarta, 2021.
  NAME <- c(
    "ACEH", "BALI", "BANTEN", "BENGKULU", "DAERAH ISTIMEWA YOGYAKARTA", "DKI JAKARTA", "GORONTALO", "JAMBI", "JAWA BARAT", "JAWA TENGAH", "JAWA TIMUR", "KALIMANTAN BARAT",
    "KALIMANTAN SELATAN", "KALIMANTAN TENGAH", "KALIMANTAN TIMUR", "KALIMANTAN UTARA", "KEPULAUAN BANGKA BELITUNG", "KEPULAUAN RIAU", "LAMPUNG", "MALUKU", "MALUKU UTARA", "NUSA TENGGARA BARAT",
    "NUSA TENGGARA TIMUR", "PAPUA", "PAPUA BARAT", "RIAU", "SULAWESI BARAT", "SULAWESI SELATAN", "SULAWESI TENGAH", "SULAWESI TENGGARA", "SULAWESI UTARA", "SUMATERA BARAT", "SUMATERA SELATAN", "SUMATERA UTARA"
  )
  pop <- c(
    5274871, 4317404, 11904562, 2010670, 3668719, 10562088, 1171681, 3548228, 48274162, 36516035, 40665696, 5414390,
    4073584, 2669969, 3766039, 701814, 1455678, 2064564, 9007848, 1848923, 1282937, 5320092,
    5325566, 4303707, 1134068, 6394087, 1419229, 9073509, 2985734, 2624875, 2621923, 5534472, 8467432, 14799361
  )
  popul <- data.frame(NAME, pop)
  Indonesiadf <- dplyr::inner_join(caseTable, popul, by = c("provinces" = "NAME"))

  # geometry
  # geomIndonesia = st_read("https://github.com/arsofyan7/Indonesia-GeoJSON/raw/master/indonesia-province.geojson") %>% select(NAME_1,geometry)
  # geomIndonesia$NAME_1[1] =  "Kepulauan Bangka Belitung"
  # geomIndonesia$NAME_1[4] =  "DKI Jakarta"
  # geomIndonesia$NAME_1[18] =  "Daerah Istimewa Yogyakarta"
  # geomIndonesia$NAME_1upper = toupper(geomIndonesia$NAME_1)
  # geomIndonesia = ms_simplify(geomIndonesia,keep=0.05,keep_shapes=TRUE)
  # data("geomIndonesia")
  geomIndonesia <- geomIndonesia %>%
    dplyr::mutate(NAME_1upper = stringr::str_to_upper(micro_name))

  # integrate datasets
  IndonesiaMap <- dplyr::inner_join(geomIndonesia, Indonesiadf, by = c("NAME_1upper" = "provinces"))
  IndonesiaMap$pInf <- IndonesiaMap$CaseDifference / IndonesiaMap$pop
  IndonesiaMap$RegionName <- paste(IndonesiaMap$micro_name, IndonesiaMap$country_name, sep = ", ")
  IndonesiaMap$Country <- IndonesiaMap$country_name

  INDONESIA_DATA <- subset(IndonesiaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(INDONESIA_DATA)
}
