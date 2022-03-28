#' getDataND
#'
#' @param code Municipality code.
#'
#' @return COVID-19 data for the Netherlands. Used in LoadNetherlands().
#' @keywords internal
getDataND <- function(code) {
  temp <- netherlandsData %>% filter(Municipality == municipality[code])
  temp$CumSum <- cumsum(temp$Cases)
  today <- temp$Date[length(temp$Date)]
  past_date <- today - 14
  pastData <- temp[temp$Date <= past_date, ]
  ### SOME ROWS DO NOT REPORT MUNICIPALITY NAME
  difference <- (temp$CumSum[length(temp$CumSum)] - pastData$CumSum[length(pastData$CumSum)]) / 14 * 10
  vec <- data.frame(Municipality = municipality[code], Code = temp$Code[1], Date = today, Difference = difference)
  return(vec)
}

#' LoadNetherlands
#'
#' @description Reads in subnational data for Netherlands to calculate most recent estimate of per capita active COVID-19 cases.
#'
#' @note
#' Covid-19 numbers per municipality as of publication date. RIVM / I & V / EPI. OSIRIS General Infectious Diseases (AIZ). \url{https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/5f6bc429-1596-490e-8618-1ed8fd768427?tab=general}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Netherlands <- LoadNetherlands()
#' }
#' @seealso [LoadCountries()]
#' @export
LoadNetherlands <- function() {
  # Covid-19 numbers per municipality as of publication date. RIVM / I & V / EPI. OSIRIS General Infectious Diseases (AIZ). https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/5f6bc429-1596-490e-8618-1ed8fd768427?tab=general

  data <- vroom("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv", delim = ";") # ,fileEncoding = 'UTF-8')
  netherlandsData <- data[, c("Date_of_publication", "Municipality_code", "Municipality_name", "Province", "Total_reported")]
  names(netherlandsData) <- c("Date", "Code", "Municipality", "Province", "Cases")
  netherlandsData$Date <- as.Date(netherlandsData$Date)
  ### Municipalities:
  municipality <- unique(netherlandsData$Municipality)
  municipality <- municipality[1:(length(municipality) - 1)]


  netherlandsTable <- data.frame()
  for (i in 1:length(municipality)) {
    vec <- getDataND(i)
    netherlandsTable <- rbind(netherlandsTable, vec)
  }

  netherlandsTable$Municipality[netherlandsTable$Municipality == "'s-Gravenhage"] <- "Des Gravenhage"

  # Note that geomNetherlands$Bevolkingsaantal is population size.

  netherlandsMap <- inner_join(geomNetherlands, netherlandsTable, by = c("micro_code" = "Code")) %>%
    inner_join(pop_netherlands, by = c("micro_code" = "Gemeentecode"))

  netherlandsMap$RegionName <- paste(netherlandsMap$micro_name, netherlandsMap$macro_name, netherlandsMap$country_name, sep = ", ")
  netherlandsMap$Country <- netherlandsMap$country_name
  netherlandsMap$DateReport <- as.character(netherlandsMap$Date)
  netherlandsMap$pInf <- netherlandsMap$Difference / netherlandsMap$Bevolkingsaantal
  NETHERLANDS_DATA <- subset(netherlandsMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf"))

  return(NETHERLANDS_DATA)
}
