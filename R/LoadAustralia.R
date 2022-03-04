#' LoadAustralia
#'
#' @return
#' @export
#'
#' @examples
LoadAustralia <- function() {
  # See: https://github.com/M3IT/COVID-19_Data/
  data <- read.csv("https://raw.githubusercontent.com/M3IT/COVID-19_Data/master/Data/COVID_AU_state_cumulative.csv")

  data$date <- as.Date(data$date)
  data <- data[rev(order(data$date)), c("date", "state", "confirmed")]
  stateList <- unique(data$state)
  today <- data$date[1]
  past <- today - 14
  australiaCases <- data.frame(Date = as.character(), state = as.character(), Difference = as.numeric())
  for (i in 1:length(stateList)) {
    difference <- (data[(data$date == today & data$state == stateList[i]), "confirmed"] - data[(data$date == past & data$state == stateList[i]), "confirmed"]) * 10 / 14
    vec <- data.frame(Date = today, state = stateList[i], Difference = difference)
    australiaCases <- rbind(australiaCases, vec)
  }


  ## population
  data("pop_australia")
  australiadf <- inner_join(australiaCases, pop_australia, by = "state")

  # geom
  # geomAustralia <- st_read('https://raw.githubusercontent.com/rowanhogan/australian-states/master/states.geojson')
  data("geomAustralia")

  AustraliaMap <- inner_join(geomAustralia, australiadf, by = c("micro_name" = "state"))
  AustraliaMap$RegionName <- paste(AustraliaMap$micro_name, AustraliaMap$country_name, sep = ", ")
  AustraliaMap$Country <- AustraliaMap$country_name
  AustraliaMap$DateReport <- as.character(AustraliaMap$Date)
  AustraliaMap$pInf <- AustraliaMap$Difference / AustraliaMap$Population
  Australia_DATA <- subset(AustraliaMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(Australia_DATA)
}

# Australia data
# COVID-19 data was obtained from https://github.com/M3IT/COVID-19_Data and aggregated by www.covid19data.com.au from local health resources.
