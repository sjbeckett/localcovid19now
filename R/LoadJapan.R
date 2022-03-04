LoadJapan <- function() {
  # Data from covid19japan.com, based on national and prefectural government reports: https://github.com/reustle/covid19japan-data/

  data <- read_json("https://raw.githubusercontent.com/reustle/covid19japan-data/master/docs/summary/latest.json", encoding = "UTF-8")
  # get updated date:
  date <- as.Date(data$updated)
  ## PREFECTURE = COUNTY
  # in data, there are 4 layers
  # take the prefecture layer to get necessary data
  dataSet <- data$prefectures
  # List the name of prefectures:
  vec <- data.frame(Prefecture = as.character(), Difference = as.numeric())
  getData <- function(i) {
    info <- unlist(dataSet[i])
    fields <- names(unlist(dataSet[i])) # get the column names
    daily <- info[startsWith(fields, "dailyConfirmedCount")] # find any column start with dailyConfirmedCount
    lenOfdaily <- length(daily) # get the index for the latest date
    past_id <- lenOfdaily - 14 # get the index for 14 days ago
    sum <- sum(as.numeric(daily[past_id:lenOfdaily])) ## get the total cases for 14 days
    difference <- round(sum * 10 / 14) # remodified
    # get the name for this prefecture
    len <- length(unlist(unlist(dataSet[i])))
    name <- as.character(unlist(as.data.frame(unlist(dataSet[i])[len])))
    temp <- c(name, difference)
    return(temp)
  }
  # get data table
  for (i in 1:49) {
    dataVec <- getData(i)
    vec <- rbind(vec, dataVec)
  }
  names(vec) <- c("Prefecture", "Difference")

  ## Geojson file
  # geomJapan <- st_read('https://raw.githubusercontent.com/deldersveld/topojson/master/countries/japan/jp-prefectures.json')
  # geomJapan <- geomJapan[c('NAME_1','geometry')]
  # geomJapan[geomJapan$NAME_1 == 'Naoasaki',1] <- "Nagasaki"
  # geomJapan = st_set_crs(geomJapan,4326)
  data("geomJapan")

  ## Population
  data("pop_japan")
  japandf <- inner_join(vec, pop_japan, by = "Prefecture")
  japandf$Difference <- as.numeric(japandf$Difference)

  JapanMap <- inner_join(geomJapan, japandf, by = c("micro_name" = "Prefecture"))
  JapanMap$RegionName <- paste(JapanMap$micro_name, JapanMap$country_name, sep = ", ")
  JapanMap$Country <- JapanMap$country_name
  JapanMap$DateReport <- as.character(date)
  JapanMap$pInf <- JapanMap$Difference / JapanMap$Population

  JAPAN_DATA <- subset(JapanMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(JAPAN_DATA)
}
