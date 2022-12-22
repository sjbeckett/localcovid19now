#' LoadVietnam
#'
#' @description Reads in subnational data for Vietnam to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Official data on the COVID-19 epidemic in Vietnam collected by the Vietnam Ministry of Health & Ministry of Information and Communications. Operated by National Center for Technology for COVID-19 Prevention and Control. Developed by VN National Cyber Security Center. \url{https://covid19.ncsc.gov.vn/dulieu}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Vietnam <- LoadVietnam()
#' }
#' @seealso [LoadData()]
#' @export
LoadVietnam <- function() {
  # Official data on the COVID-19 epidemic in Vietnam collected by the Vietnam Ministry of Health & Ministry of Information and Communications. Operated by National Center for Technology for COVID-19 Prevention and Control. Developed by VN National Cyber Security Center. https://covid19.ncsc.gov.vn/dulieu

  # provincedata <- read_json("https://covid19.ncsc.gov.vn/api/v3/covid/provinces")
  # extract relevant province information
  # ProvinceInfo=c()
  # for(aa in 1:length(provincedata)){
  # ProvinceInfo$id[aa] = provincedata[[aa]]$id
  # ProvinceInfo$Name[aa] = provincedata[[aa]]$name
  # ProvinceInfo$Population[aa] = provincedata[[aa]]$population
  # }
  # ProvinceInfo=data.frame(ProvinceInfo)
  # write.csv(ProvinceInfo,"countries/data/VietnamProvinceInfo.csv",row.names = FALSE)
  # ProvinceInfo <- read.csv("countries/data/VietnamProvinceInfo.csv")

  misc_vietnam <- geomVietnam <- NULL
  utils::data("misc_vietnam", envir = environment())
  utils::data("geomVietnam", envir = environment())
  geomVietnam <- sf::st_as_sf(geomVietnam)

  # ProvinceInfo <- misc_vietnam
  # Geometry
  # geom = st_read("https://github.com/hausuresh/vietnam-geocode/raw/master/vietnam.geojson")
  # geom$name[18] = ProvinceInfo$Name[57]
  # geom$name[53] = ProvinceInfo$Name[2]
  # geom$name[59] = ProvinceInfo$Name[40]
  # st_write(geom,"geomVietnam.geojson")
  # geomVietnam <- st_read("countries/data/geom/geomVietnam.geojson")
  # need to do some work to get the naming to match up
  misc_vietnam$NameUse <- geomVietnam$micro_name[misc_vietnam$code]

  # look at case data
  data <- jsonlite::read_json("https://covid19.ncsc.gov.vn/api/v3/covid/provinces?filter_type=case_by_time")
  provinceID <- names(data)

  CaseDiff <- c()
  DateReport <- c()
  code <- c()

  # most recent data has same cumulative entries as the prior day. Suggest not using the most recent day in calculations.
  for (aa in 1:length(provinceID)) {
    MostRecent <- utils::tail(data[[aa]], 15)
    currentCases <- MostRecent[14]
    pastCases <- MostRecent[1]
    CaseDiff[aa] <- (10 / 14) * (currentCases[[1]] - pastCases[[1]])
    DateReport[aa] <- utils::tail(names(data[[2]]), 2)[1]
    code[aa] <- as.numeric(provinceID[aa])
  }
  # format date
  DateReport <- as.character(as.Date(DateReport, format = "%d/%m/%Y"))
  dataTable <- data.frame(DateReport = DateReport, Code = code, Difference = CaseDiff)

  datadf <- dplyr::inner_join(dataTable, misc_vietnam, by = c("Code" = "id"))


  VietnamMap <- dplyr::inner_join(geomVietnam, datadf, by = c("micro_name" = "NameUse"))
  VietnamMap$Country <- "Vietnam"
  VietnamMap$RegionName <- paste(VietnamMap$micro_name, VietnamMap$country_name, sep = ", ")
  VietnamMap$pInf <- as.numeric(VietnamMap$Difference) / as.numeric(VietnamMap$Population)
  Vietnam_DATA <- subset(VietnamMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(Vietnam_DATA)
}
