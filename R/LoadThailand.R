#' LoadThailand
#'
#' @description Reads in subnational data for Thailand to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Thailand Covid testing and case data gathered and combined from various sources for others to download or view:  \url{https://djay.github.io/covidthailand}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Thailand <- LoadThailand()
#' }
#' @seealso [LoadData()]
#' @export
LoadThailand <- function() {
  # Thailand Covid testing and case data gathered and combined from various sources for others to download or view:  https://djay.github.io/covidthailand
  geomThailand <- pop_thailand <- misc_thailand <- micro_code <- NULL
  utils::data(list = c("geomThailand", "pop_thailand", "misc_thailand"), envir = environment())
  geomThailand <- sf::st_as_sf(geomThailand)
  
  # cases
  cases <- utils::read.csv("https://raw.githubusercontent.com/wiki/djay/covidthailand/cases_by_province.csv") # new cases per day

  provinces <- unique(cases$Province)
  DateReport <- c()
  CaseDifference <- c()
  for (aa in 1:length(provinces)) {
    subsetdata <- cases[which(cases$Province == provinces[aa]), ]
    DateReport[aa] <- max(subsetdata$Date)
    CaseDifference[aa] <- 10 / 14 * sum(subsetdata$Cases[which(as.Date(subsetdata$Date) > (as.Date(DateReport[aa]) - 14))])
  }
  caseTable <- data.frame(provinces, DateReport, CaseDifference)


  # pop
  # popul = read.csv("https://github.com/djay/covidthailand/raw/main/province_mapping.csv")%>%select(c("Name",pop = "Population..2019..1."))
  # write.csv("countries/data/thailand_pop.csv",row.names = FALSE)
  # popul <- read.csv("countries/data/thailand_pop.csv")
  Thailanddf <- dplyr::inner_join(caseTable, pop_thailand, by = c("provinces" = "Name"))

  # geometry
  # geomThai = st_read("https://github.com/chingchai/OpenGISData-Thailand/raw/master/provinces.geojson")
  # geomThai$pro_en[geomThai$pro_en=="Buri Ram"] = "Buriram"
  # geomThai$pro_en[geomThai$pro_en=="Chon Buri"] = "Chonburi"
  # geomThai$pro_en[geomThai$pro_en=="Lop Buri"] = "Lopburi"
  # geomThai$pro_en[geomThai$pro_en=="Nong Bua Lam Phu"] = "Nong Bua Lamphu"
  # geomThai$pro_en[geomThai$pro_en=="Phang-nga"] = "Phang Nga"
  # geomThai$pro_en[geomThai$pro_en=="Prachin Buri"] = "Prachinburi"
  # geomThai$pro_en[geomThai$pro_en=="Sa kaeo"] = "Sa Kaeo"
  # geomThai$pro_en[geomThai$pro_en=="Si Sa Ket"] = "Sisaket"
  # geomThai$pro_en[geomThai$pro_en=="Samut Prakarn"] = "Samut Prakan"
  # geomThailand <- st_read("countries/data/geom/geomThailand.geojson")
  # miscThailand <- vroom("countries/data/miscThailand.csv", col_types = cols(pro_code = col_character()))

  ThailandMap <- dplyr::inner_join(geomThailand, Thailanddf, by = c("micro_name" = "provinces")) %>%
    dplyr::mutate(micro_code = as.numeric(micro_code)) %>%
    dplyr::inner_join(misc_thailand, by = c("micro_code" = "pro_code"))
  
  
  ThailandMap$pInf <- ThailandMap$CaseDifference / ThailandMap$pop
  ThailandMap$RegionName <- paste(paste(ThailandMap$micro_name, ThailandMap$pro_th, sep = "/"), ThailandMap$country_name, sep = ", ")
  ThailandMap$Country <- ThailandMap$country_name
  THAILAND_DATA <- subset(ThailandMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(THAILAND_DATA)
}
