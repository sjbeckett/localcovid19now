# calculate risk
estRisk <- function(Infected, A, G) { # A is ascertainment bias, G is group size
  Risk <- 100 * (1 - (1 - (A * Infected))^G)
  return(round(Risk, 0))
}


## create risk-maps and active case per capita maps
#Leaflet
EventMap_leaflet <- function(DATA, G) { # DATA - map data, G - group size

  DATA$risk <- estRisk(DATA$pInf, DATA$AB, G)
  MMap <- DATA
  MMap$riskLabels <- MMap$risk
  MMap <- MMap %>%
    mutate(riskLabels = case_when(
      riskLabels > 99 ~ "> 99",
      riskLabels < 1 ~ "< 1",
      riskLabels < 0 ~ "NA",
      is.na(riskLabels) ~ "No Data",
      TRUE ~ as.character(riskLabels)
    ))
  bins <- c(0, 5, 25, 50, 75, 95, 100)
  legendlabs <<- c("< 5", " 1-25", "25-50", "50-75", "75-99", "> 95")
  pal <- colorBin("YlOrRd", domain = MMap$risk, bins = bins, na.color = "grey")
  JAM <- leafletOptions(worldCopyJump = TRUE)


  labels <- sprintf(
    "<strong>Region: %s</strong><br/>Risk Score: %s%%<br/>Ascertainment Bias: %s<br/>Updated Date: %s<br/>",
    MMap$RegionName, MMap$riskLabels, round(MMap$AB, 2), MMap$DateReport
  ) %>% lapply(htmltools::HTML)
  leaflet(MMap, options = JAM) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = MMap,
      fillColor = ~ pal(risk),
      weight = 0.05,
      opacity = 0.8,
      color = "black",
      dashArray = "2",
      fillOpacity = 0.8, smoothFactor = 0.1,
      highlight = highlightOptions(
        weight = 0,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.6,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal, values = ~risk, opacity = 0.9, title = "Risk Level (%)", na.label = "NA",
      position = "bottomleft", labFormat = labelFormat()
    )
}


EventMapB_leaflet <- function(DATA, G) { # map data, Ascertainment bias, Group size

  DATA$risk <- estRisk(DATA$pInf, DATA$AB, G)
  MMap <- DATA
  MMap$riskLabels <- MMap$risk
  MMap <- MMap %>%
    mutate(riskLabels = case_when(
      riskLabels > 99 ~ "> 99",
      riskLabels < 1 ~ "< 1",
      riskLabels < 0 ~ "NA",
      is.na(riskLabels) ~ "No Data",
      TRUE ~ as.character(riskLabels)
    ))
  bins <- c(0, 5, 25, 50, 75, 95, 100)
  legendlabs <<- c("< 5", " 1-25", "25-50", "50-75", "75-99", "> 95")
  pal <- colorBin("YlOrRd", domain = MMap$risk, bins = bins, na.color = "grey")
  JAM <- leafletOptions(worldCopyJump = TRUE)


  labels <- sprintf(
    "<strong>Region: %s</strong><br/>Risk Score: %s%%<br/>Ascertainment Bias: %s<br/>Updated Date: %s<br/>",
    MMap$RegionName, MMap$riskLabels, round(MMap$AB, 2), MMap$DateReport
  ) %>% lapply(htmltools::HTML)
  leaflet(MMap, options = JAM) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = MMap,
      fillColor = ~ pal(risk),
      weight = 0.5,
      opacity = 0.8,
      color = "black",
      dashArray = "2",
      fillOpacity = 0.8, smoothFactor = 0.1,
      highlight = highlightOptions(
        weight = 0,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.6,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal, values = ~risk, opacity = 0.9, title = "Risk Level (%)", na.label = "NA",
      position = "bottomleft", labFormat = labelFormat()
    )
}


PerCapitaMap_leaflet <- function(DATA, people) { # DATA - map data, people - active cases per people

  DATA$percapcases <- DATA$pInf*people
  MMap <- DATA

  bins <- c(0, 5, 25, 50, 75, 95, 100)
  legendlabs <<- c("< 5", " 1-25", "25-50", "50-75", "75-99", "> 95")
  pal <- colorBin("YlOrRd", domain = MMap$percapcases, na.color = "grey")
  JAM <- leafletOptions(worldCopyJump = TRUE)

  labels <- sprintf(
    "<strong>Region: %s</strong><br/>Active cases per %s people: %s<br/>Updated Date: %s<br/>",
    MMap$RegionName, prettyNum(people,big.mark=",",scientific=FALSE), round(MMap$percapcases, 2), MMap$DateReport
  ) %>% lapply(htmltools::HTML)
  leaflet(MMap, options = JAM) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = MMap,
      fillColor = ~ pal(percapcases),
      weight = 0.05,
      opacity = 0.8,
      color = "black",
      dashArray = "2",
      fillOpacity = 0.8, smoothFactor = 0.1,
      highlight = highlightOptions(
        weight = 0,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.6,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal, values = ~percapcases, opacity = 0.9, title = paste("Active cases per",prettyNum(people,big.mark=",",scientific=FALSE),"people"), na.label = "NA",
      position = "bottomleft", labFormat = labelFormat()
    )
}


PerCapitaMapB_leaflet <- function(DATA, people) { # DATA - map data, people - active cases per people

  DATA$percapcases <- DATA$pInf*people
  MMap <- DATA

  bins <- c(0, 5, 25, 50, 75, 95, 100)
  legendlabs <<- c("< 5", " 1-25", "25-50", "50-75", "75-99", "> 95")
  pal <- colorBin("YlOrRd", domain = MMap$percapcases, na.color = "grey")
  JAM <- leafletOptions(worldCopyJump = TRUE)

  labels <- sprintf(
    "<strong>Region: %s</strong><br/>Active cases per %s people: %s<br/>Updated Date: %s<br/>",
    MMap$RegionName, prettyNum(people,big.mark=",",scientific=FALSE), round(MMap$percapcases, 2), MMap$DateReport
  ) %>% lapply(htmltools::HTML)
  leaflet(MMap, options = JAM) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = MMap,
      fillColor = ~ pal(percapcases),
      weight = 0.5,
      opacity = 0.8,
      color = "black",
      dashArray = "2",
      fillOpacity = 0.8, smoothFactor = 0.1,
      highlight = highlightOptions(
        weight = 0,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.6,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal, values = ~percapcases, opacity = 0.9, title = paste("Active cases per",prettyNum(people,big.mark=",",scientific=FALSE),"people"), na.label = "NA",
      position = "bottomleft", labFormat = labelFormat()
    )
}


##tmap
EventMap_tmap <- function(DATA,G){
	data("World")
	#use equal earth projection
	DATA = st_transform(DATA, crs = "+proj=eqearth")

	DATA$risk <- estRisk(DATA$pInf,DATA$AB,G) 
	bins <- c(0, 5, 25, 50, 75, 95, 100)
	pal <- colorBin("YlOrRd", domain = DATA$risk, bins = bins, na.color = "grey")

	tm_shape(DATA) + 
		tm_polygons(col="risk", id = "RegionName", title=paste("Risk (%) of exposure to\nCOVID-19 in a group of",G), border.col = "lightgrey", border.alpha=0.2,lwd=0.005,palette = pal(bins),breaks=bins) +
		tm_shape(World) +
		tm_layout(legend.outside=TRUE,legend.outside.position="right")+
		tm_borders("grey", lwd = .5) +
		tm_text("iso_a3", size = "AREA")
}



EventMapB_tmap <- function(DATA,G){
	data("World")
	#use equal earth projection
	DATA = st_transform(DATA, crs = "+proj=eqearth")

	DATA$risk <- estRisk(DATA$pInf,DATA$AB,G) 
	bins <- c(0, 5, 25, 50, 75, 95, 100)
	pal <- colorBin("YlOrRd", domain = DATA$risk, bins = bins, na.color = "grey")

	tm_shape(DATA) + 
		tm_polygons(col="risk", id = "RegionName", title=paste("Risk (%) of exposure to\nCOVID-19 in a group of",G), border.col = "lightgrey", border.alpha=.8,lwd=1.5,palette = pal(bins),breaks=bins) +
		tm_shape(World) +
		tm_layout(legend.outside=TRUE,legend.outside.position="right")+
		tm_borders("grey", lwd = .5) +
		tm_text("iso_a3", size = "AREA")
}



PerCapitaMap_tmap <- function(DATA,people){
	data("World")
	#use equal earth projection
	DATA = st_transform(DATA, crs = "+proj=eqearth")

	DATA$percapcases <- DATA$pInf*people

	tm_shape(DATA) + 
		tm_polygons(col="percapcases", id = "geoid", title=paste("Active cases per",prettyNum(people,big.mark=",",scientific=FALSE),"people"), border.col = "lightgrey", border.alpha=0.2,lwd=0.005) +
		tm_shape(World)+
		tm_layout(legend.outside=TRUE,legend.outside.position="right")+
		tm_borders("grey", lwd = .5) +
		tm_text("iso_a3", size = "AREA")
}


PerCapitaMapB_tmap <- function(DATA,people){
	data("World")
	#use equal earth projection
	DATA = st_transform(DATA, crs = "+proj=eqearth")

	DATA$percapcases <- DATA$pInf*people

	tm_shape(DATA) + 
		tm_polygons(col="percapcases", id = "geoid", title=paste("Active cases per",prettyNum(people,big.mark=",",scientific=FALSE),"people"), border.col = "lightgrey", border.alpha=0.8,lwd=1.5) +
		tm_shape(World)+
		tm_layout(legend.outside=TRUE,legend.outside.position="right")+
		tm_borders("grey", lwd = .5) +
		tm_text("iso_a3", size = "AREA")
}

