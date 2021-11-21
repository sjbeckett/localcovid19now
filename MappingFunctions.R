#calculate risk
estRisk <- function(Infected,A,G){#A is ascertainment bias, G is group size
  Risk = 100*(1-(1-(A*Infected))^G)
  return(round(Risk,0))
}


#create risk-maps
EventMap <- function(DATA,G){ #DATA - map data, G - group size
  
  DATA$risk <- estRisk(DATA$pInf,DATA$AB,G) 
  MMap = DATA
  MMap$riskLabels <- MMap$risk
  MMap <- MMap %>%
    mutate(riskLabels = case_when(
      riskLabels > 99 ~ '> 99',
      riskLabels < 1 ~ '< 1',
      riskLabels < 0 ~ 'NA',
      is.na(riskLabels) ~ 'No Data',
      TRUE ~ as.character(riskLabels)
    ))
  bins <- c(0,5,25,50,75,95,100)
  legendlabs <<- c("< 5", " 1-25", "25-50", "50-75", "75-99", "> 95")
  pal <- colorBin("YlOrRd", domain = MMap$risk, bins = bins, na.color = 'grey')
  JAM = leafletOptions(worldCopyJump=TRUE)
  
  
  labels <- sprintf(
    "<strong>Region: %s</strong><br/>Risk Score: %s%%<br/>Ascertainment Bias: %s<br/>Updated Date: %s<br/>",
    MMap$RegionName, MMap$riskLabels, round(MMap$AB,2), MMap$DateReport
  ) %>% lapply(htmltools::HTML)
  leaflet(MMap,options=JAM) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
	addPolygons(data = MMap,
                fillColor = ~pal(risk),
                weight = 0.05,
                opacity = 0.8,
                color = "black",
                dashArray = "2",
                fillOpacity = 0.8, smoothFactor=0.1,
                highlight = highlightOptions(
                  weight = 0,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.6,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addLegend(pal = pal, values = ~risk, opacity = 0.9, title = 'Risk Level (%)', na.label = 'NA',
              position = "bottomleft",labFormat = labelFormat())
}


#create risk-maps
EventMapB <- function(DATA,G){ #map data, Ascertainty bias, Group size
  
  DATA$risk <- estRisk(DATA$pInf,DATA$AB,G) 
  MMap = DATA
  MMap$riskLabels <- MMap$risk
  MMap <- MMap %>%
    mutate(riskLabels = case_when(
      riskLabels > 99 ~ '> 99',
      riskLabels < 1 ~ '< 1',
      riskLabels < 0 ~ 'NA',
      is.na(riskLabels) ~ 'No Data',
      TRUE ~ as.character(riskLabels)
    ))
  bins <- c(0,5,25,50,75,95,100)
  legendlabs <<- c("< 5", " 1-25", "25-50", "50-75", "75-99", "> 95")
  pal <- colorBin("YlOrRd", domain = MMap$risk, bins = bins, na.color = 'grey')
  JAM = leafletOptions(worldCopyJump=TRUE)
  
  #pal <- colorNumeric(
	#palette = "viridis",
	#domain = c(0,100))

  
  labels <- sprintf(
    "<strong>Region: %s</strong><br/>Risk Score: %s%%<br/>Ascertainment Bias: %s<br/>Updated Date: %s<br/>",
    MMap$RegionName, MMap$riskLabels, round(MMap$AB,2), MMap$DateReport
  ) %>% lapply(htmltools::HTML)
  leaflet(MMap,options=JAM) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
	addPolygons(data = MMap,
                fillColor = ~pal(risk),
                weight = 0.5,
                opacity = 0.8,
                color = "black",
                dashArray = "2",
                fillOpacity = 0.8,smoothFactor=0.1,
                highlight = highlightOptions(
                  weight = 0,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.6,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addLegend(pal = pal, values = ~risk, opacity = 0.9, title = 'Risk Level (%)', na.label = 'NA',
              position = "bottomleft",labFormat = labelFormat())
}
