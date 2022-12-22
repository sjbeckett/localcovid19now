#' LoadChina
#'
#' @description Reads in subnational data for China to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' COVID-19 data for first-level administrative divisions in China is aggregated from the
#' National Health Commission of the People’s Republic of China (NHC): \url{http://www.nhc.gov.cn/xcs/yqtb/list_gzbd.shtml} and the
#' China CDC (CCDC): \url{http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm}
#' by the COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University
#' \url{https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data}.
#'
#' @references
#' Dong, E., Du, H., & Gardner, L. (2020). An interactive web-based dashboard to track COVID-19 in real time. The Lancet infectious diseases, 20(5), 533-534.
#'
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' China <- LoadChina()
#' @seealso [LoadData()]
#' @export
LoadChina <- function() {
  # COVID-19 data for first-level administrative divisions in China is aggregated from
  #    National Health Commission of the People’s Republic of China (NHC): http://www.nhc.gov.cn/xcs/yqtb/list_gzbd.shtml
  #    China CDC (CCDC): http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm
  # by the COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University
  # https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
  # Dong, E., Du, H., & Gardner, L. (2020). An interactive web-based dashboard to track COVID-19 in real time. The Lancet infectious diseases, 20(5), 533-534.

  pop_china <- NULL
  data("geomChina", envir = environment())
  geomChina <- sf::st_as_sf(geomChina)
  data("pop_china", envir = environment())
  # load cases data
  data <- utils::read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

  # get updated date:
  date <- names(data)[length(names(data))]
  date <- strsplit(date, "X")[[1]][2]
  date <- as.Date(date, format = "%m.%d.%y")

  chinaData <- data[data$Country.Region == "China", ]
  len <- length(names(chinaData))
  latestData <- chinaData[, c(1, (len - 14), len)]
  names(latestData) <- c("Province", "Past", "Today")
  latestData[4] <- (latestData[3] - latestData[2]) * 10 / 14
  latestData <- latestData[, c(1, 4)]
  names(latestData) <- c("Province", "Difference")
  ## population file
  chinadf <- dplyr::inner_join(latestData, pop_china, by = c("Province" = "Name"))

  ## geojson file
  # geomChina <- st_read('https://raw.githubusercontent.com/stjacob/china_geojson/master/china.geojson')

  # alter naming to match across datasets
  geomChina$micro_name[which(geomChina$micro_name == "Nei Mongol")] <- "Inner Mongolia"
  geomChina$micro_name[which(geomChina$micro_name == "Ningxia Hui")] <- "Ningxia"
  geomChina$micro_name[which(geomChina$micro_name == "HongKong")] <- "Hong Kong"
  geomChina$micro_name[which(geomChina$micro_name == "Xinjiang Uygur")] <- "Xinjiang"
  geomChina$micro_name[which(geomChina$micro_name == "Xizang")] <- "Tibet"


  # integrate datasets
  ChinaMap <- dplyr::inner_join(geomChina, chinadf, by = c("micro_name" = "Province"))
  ChinaMap$RegionName <- paste0(ChinaMap$micro_name, ", China")
  ChinaMap$Country <- ChinaMap$country_name
  ChinaMap$DateReport <- as.character(date)
  ChinaMap$pInf <- ChinaMap$Difference / ChinaMap$Population_2020

  CHINA_DATA <- subset(ChinaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(CHINA_DATA)
}
