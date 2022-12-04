#' LoadIndia
#'
#' @description Reads in subnational data for India to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data collated by \url{https://covid19tracker.in/}, an initiative of the Indian Institute of Technology Hyderabad, from state bulletins and official reports with thanks to the covid19india.org team for their outstanding work in creating the original portal, and for making their code base public.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' India <- LoadIndia()
#' }
#' @seealso [LoadData()]
#' @export
LoadIndia <- function() {
  # Data collated by https://covid19tracker.in/, an initiative of the Indian Institute of Technology Hyderabad, from state bulletins and official reports
  # with thanks to the covid19india.org team for their outstanding work in creating the original portal, and for making their code base public.

  pop_india <- geomIndia <- NULL
  utils::data(list = c("geomIndia", "pop_india"), envir = environment())
  geomIndia <- sf::st_as_sf(geomIndia)

  data <- jsonlite::read_json("https://api.covid19tracker.in/data/static/timeseries.min.json")
  # UN - unknown; TT - total for India

  stateList <- names(data)
  dataTable <- data.frame(Date = as.character(), Code = as.character(), Difference = as.numeric())
  for (i in 1:length(stateList)) {
    state <- stateList[i]
    lenOfDay <- length(names(data[i][[1]]$dates))
    pastIndex <- lenOfDay - 14
    date <- names(data[i][[1]]$dates[lenOfDay])
    ## there may be some null data for confirmed for current day or 14 days ago
    if (!is.null(data[i][[1]]$dates[lenOfDay][[1]]$total$confirmed) && !is.null(data[i][[1]]$dates[pastIndex][[1]]$total$confirmed)) {
      currentCases <- data[i][[1]]$dates[lenOfDay][[1]]$total$confirmed
      pastCases <- data[i][[1]]$dates[pastIndex][[1]]$total$confirmed
      difference <- (currentCases - pastCases) / 14 * 10
    } else {
      difference <- NA
    }
    vec <- data.frame(Date = date, Code = state, Difference = difference)
    dataTable <- rbind(dataTable, vec)
  }


  ## population file
  # pop <- read_json('https://api.covid19india.org/misc.json')
  # populationTable <- data.frame(Code = as.character(), State = as.character(), Population = as.numeric())
  # for (i in 1:length(pop$state_meta_data)){
  #  code <- pop$state_meta_data[[i]]$abbreviation # take the state code
  #  population <- pop$state_meta_data[[i]]$population # extract the corresponding population
  #  state <- pop$state_meta_data[[i]]$stateut
  #  vec <- data.frame(Code = code, State = state, Population = population)
  #  populationTable <- rbind(populationTable,vec)
  # }
  # write.csv(populationTable,"popIndia.csv",row.names = FALSE)
  # data("pop_india")


  indiadf <- dplyr::inner_join(dataTable, pop_india, by = c("Code"))



  # GEOGRAPHIC GEOJSON FILE
  # First effort
  # geomIndia <- st_read("https://github.com/india-in-data/india-states-2019/raw/master/india_states.geojson")
  # sptemp = geomIndia
  # sptemp = st_make_valid(sptemp)
  # sptemp$ST_NM[which(sptemp$ID=="AN")] = "Andaman and Nicobar Islands"
  # sptemp$ST_NM[which(sptemp$ID=="DL")] = "Delhi"
  # sptemp$ST_NM[which(sptemp$ID=="JK")] ="Jammu and Kashmir"
  # sptemp$ST_NM[which(sptemp$ID=="DN")] = "Dadra and Nagar Haveli and Daman and Diu"
  # sptemp$ST_NM[which(sptemp$ID=="DD")] = "Dadra and Nagar Haveli and Daman and Diu"
  # sptemp$ID[which(sptemp$ID=="DN")] = "DNDD"
  # sptemp$ID[which(sptemp$ID=="DD")] = "DNDD"
  # sptemp = sptemp %>%
  #    group_by(ST_NM,ID) %>%
  #    summarise(geometry = sf::st_union(geometry)) %>%
  # 	ungroup()
  # sptemp = st_cast(sptemp,"MULTIPOLYGON")
  # st_write(sptemp,"geomIndia.geojson")
  # data("geomIndia")

  # Second effort to account for non-India administrated regions in Kashmir.
  # geomIndia <- st_read("https://github.com/india-in-data/india-states-2019/raw/master/india_states.geojson")
  # sptemp = geomIndia
  # sptemp = st_make_valid(sptemp)
  # sptemp$ST_NM[which(sptemp$ID=="AN")] = "Andaman and Nicobar Islands"
  # sptemp$ST_NM[which(sptemp$ID=="DL")] = "Delhi"
  # sptemp$ST_NM[which(sptemp$ID=="JK")] ="Jammu and Kashmir"
  # sptemp$ST_NM[which(sptemp$ID=="DN")] = "Dadra and Nagar Haveli and Daman and Diu"
  # sptemp$ST_NM[which(sptemp$ID=="DD")] = "Dadra and Nagar Haveli and Daman and Diu"
  # sptemp$ID[which(sptemp$ID=="DN")] = "DNDD"
  # sptemp$ID[which(sptemp$ID=="DD")] = "DNDD"
  # sptemp = sptemp %>%
  #    dplyr::group_by(ST_NM,ID) %>%
  #   dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  # 	dplyr::ungroup()
  #
  # geomKashmir_JK <- st_make_valid(st_read("https://raw.githubusercontent.com/hamadkh/GeoJson4Kashmir/master/J%26K/jammukashmirdistricts.geo.json"))
  #
  # INDs = c(which(geomKashmir_JK$id=="kargil"),which(geomKashmir_JK$id=="leh"))
  # AdminLadakh = geomKashmir_JK[INDs,] %>%
  #   st_union() %>%
  #   st_sf()
  # AdminLadakh$geometry[[1]][[4]] = NULL
  # AdminLadakh$geometry[[1]][[3]] = NULL
  # AdminLadakh$geometry[[1]][[2]] = NULL
  #
  # AdminJK = geomKashmir_JK[-c(INDs,which(geomKashmir_JK$id=="Haveli")),] %>% st_transform(crs = 7764) %>% st_buffer(100) %>%
  #  st_union() %>% st_transform(crs = 4326) %>%
  #  st_sf()
  #
  # sptemp$geometry[which(sptemp$ID=="LA")] = AdminLadakh$geometry
  # sptemp$geometry[which(sptemp$ID=="JK")] = AdminJK$geometry
  # sptemp = st_cast(sptemp,"MULTIPOLYGON")
  # st_write(sptemp,"geomIndia.geojson")

  indiaMap <- dplyr::inner_join(geomIndia, indiadf, by = c("micro_name" = "State"))
  indiaMap$DateReport <- as.character(indiaMap$Date)
  indiaMap$RegionName <- paste(indiaMap$micro_name, indiaMap$country_name, sep = ", ")
  indiaMap$Country <- indiaMap$country_name
  indiaMap$pInf <- as.numeric(indiaMap$Difference) / as.numeric(indiaMap$Population)
  india_DATA <- subset(indiaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(india_DATA)
}
