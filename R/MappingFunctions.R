#' calculate risk
#'
#' @description Calculates the percentage probability that one or more persons in a group, with size G, may be infectious given the underlying prevalence of disease.
#'
#' @param pcActiveCases Per capita active cases.
#' @param A Ascertainment bias (ratio of infections to cases)
#' @param G Event size for risk to be calculated for.
#' @param rounding Number of decimal places to round to. Default of 0.
#'
#' @return Returns the risk that one or more people at an event size G may be infectious given case prevalence and ascertainment bias parameters.
#'
#' @seealso [calcRisk()]
#' @examples
#' # estimated risk that one or more are infectious in a group of 100,
#' # when there are 50 active cases in population of 10,000 and cases
#' # are underascertained by a factor of 4.
#' estRisk(50 / 10000, 4, 100)
#' @export
estRisk <- function(pcActiveCases, A, G, rounding = 0) {
  stopifnot("`A` must be a positive value." = is.numeric(A) & A>0)
  stopifnot("`G` must be a positive value." = is.numeric(G) & G>0)
  stopifnot("`rounding` must be numeric." = is.numeric(rounding))
  # A is ascertainment bias, G is group size
  Risk <- 100 * (1 - (1 - (A * pcActiveCases))^G)
  return(round(Risk, rounding))
}


## create risk-maps and active case per capita maps in Leaflet and tmap

#' EventMap_leaflet
#'
#' @description Creates an interactive Leaflet map displaying exposure risk for the input data.
#'
#' @param DATA Data containing prevalence information to map.
#' @param G Event size to compute risk for.
#' @param AB Case ascertainment bias to compute risk for.
#' @param boundaryweights Weight assigned to the maps boundary edges.
#'
#' @return Outputs an interactive leaflet map displaying exposure risk for the input data.
#'
#' @family mapplots
#' @examples
#' \dontrun{
#' Austria <- LoadData("LoadAustria")
#' Austria$AB <- 4
#' EventMap_leaflet(Austria, 50, Austria$AB)
#' }
#' @export
EventMap_leaflet <- function(DATA, G, AB, boundaryweights = 0.05) { # DATA - map data, G - group size, AB - case ascertainment bias, boundaryweights - polygon edge weights

  rlang::check_installed(c("leaflet", "RColorBrewer"), reason = "to use `EventMap_leaflet()`")

  DATA$risk <- estRisk(DATA$pInf, AB, G)
  MMap <- DATA
  MMap$riskLabels <- MMap$risk
  MMap <- MMap %>%
    dplyr::mutate(riskLabels = dplyr::case_when(
      riskLabels > 99 ~ "> 99",
      riskLabels < 1 ~ "< 1",
      riskLabels < 0 ~ "NA",
      is.na(riskLabels) ~ "No Data",
      TRUE ~ as.character(riskLabels)
    ))
  bins <- c(0, 5, 25, 50, 75, 95, 100)
  legendlabs <- c("< 5", " 1-25", "25-50", "50-75", "75-99", "> 95")
  pal <- leaflet::colorBin("YlOrRd", domain = MMap$risk, bins = bins, na.color = "grey")
  JAM <- leaflet::leafletOptions(worldCopyJump = TRUE)


  labels <- sprintf(
    "<strong>Region: %s</strong><br/>Risk Score: %s%%<br/>Ascertainment Bias: %s<br/>Updated Date: %s<br/>",
    MMap$RegionName, MMap$riskLabels, round(MMap$AB, 2), MMap$DateReport
  ) %>% lapply(htmltools::HTML)
  leaflet::leaflet(MMap, options = JAM) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(
      data = MMap,
      fillColor = ~ pal(risk),
      weight = boundaryweights,
      opacity = 0.8,
      color = "black",
      dashArray = "2",
      fillOpacity = 0.8, smoothFactor = 0.1,
      highlight = leaflet::highlightOptions(
        weight = 0,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.6,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    leaflet::addLegend(
      pal = pal, values = ~risk, opacity = 0.9, title = "Risk Level (%)", na.label = "NA",
      position = "bottomleft", labFormat = leaflet::labelFormat()
    )
}

#' PerCapitaMap_leaflet
#'
#' @description Creates an interactive Leaflet map displaying active cases per 'people' for the input data.
#'
#' @param DATA Data containing prevalence information to map.
#' @param people Transform from proportion of population to per 'people'.
#' @param boundaryweights Weight assigned to the maps boundary edges.
#'
#' @return Outputs an interactive leaflet map displaying active cases per 'people' for the input data.
#'
#' @family mapplots
#' @examples
#' \dontrun{
#' Denmark <- LoadData("LoadDenmark")
#' PerCapitaMap_leaflet(Denmark)
#' }
#' @export
PerCapitaMap_leaflet <- function(DATA, people = 100000, boundaryweights = 0.05) { # DATA - map data, people - transform from proportion of population to per 'people', boundaryweights - polygon edge weights
  rlang::check_installed(c("leaflet", "RColorBrewer"), reason = "to use `PerCapitaMap_leaflet()`")

  DATA$percapcases <- DATA$pInf * people
  MMap <- DATA

  bins <- 0.01 * c(0, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 100) * people
  pal <- leaflet::colorBin("YlOrRd", domain = MMap$percapcases, na.color = "grey", bins = bins)
  JAM <- leaflet::leafletOptions(worldCopyJump = TRUE)

  labels <- sprintf(
    "<strong>Region: %s</strong><br/>Active cases per %s people: %s<br/>Updated Date: %s<br/>",
    MMap$RegionName, prettyNum(people, big.mark = ",", scientific = FALSE), round(MMap$percapcases, 2), MMap$DateReport
  ) %>% lapply(htmltools::HTML)
  leaflet::leaflet(MMap, options = JAM) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(
      data = MMap,
      fillColor = ~ pal(percapcases),
      weight = boundaryweights,
      opacity = 0.8,
      color = "black",
      dashArray = "2",
      fillOpacity = 0.8, smoothFactor = 0.1,
      highlight = leaflet::highlightOptions(
        weight = 0,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.6,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    leaflet::addLegend(
      pal = pal, values = ~percapcases, opacity = 0.9, title = paste("Active cases per", prettyNum(people, big.mark = ",", scientific = FALSE), "people"), na.label = "NA",
      position = "bottomleft", labFormat = leaflet::labelFormat()
    )
}


## tmap

#' EventMap_tmap
#'
#' @description Creates a tmap map displaying active cases per 'people' for the input data.
#'
#' @param DATA Data containing prevalence information to map.
#' @param G Event size to compute risk for.
#' @param AB Case ascertainment bias to compute risk for.
#' @param boundaryweights Weight assigned to the maps boundary edges.
#' @param projectionCRS Type of geographic projection to use.
#' @param maptitle Adds title to map.
#'
#' @return Outputs a tmap displaying exposure risk for the input data.
#'
#' @family mapplots
#' @examples
#' \dontrun{
#' Austria <- LoadData("LoadAustria")
#' Austria$AB <- 4
#' EventMap_tmap(Austria, 50, Austria$AB)
#' }
#' @export
EventMap_tmap <- function(DATA, G, AB, boundaryweights = 0.05, projectionCRS = "+proj=eqearth", maptitle = NA) { # DATA - map data, G - group size, AB - case ascertainment bias, boundaryweights - polygon edge weights, projectionCRS - type of geographic projection to use, maptitle - adds a title to the map.

  rlang::check_installed("tmap", reason = "to use EventMap_tmap()")
  World <- NULL
  utils::data("World", package = "tmap", envir = environment())
  # use equal earth projection
  DATA <- sf::st_transform(DATA, crs = projectionCRS)

  DATA$risk <- estRisk(DATA$pInf, AB, G)
  bins <- c(0, 5, 25, 50, 75, 95, 100)
  pal <- leaflet::colorBin("YlOrRd", domain = DATA$risk, bins = bins, na.color = "grey")

  tmap::tm_shape(DATA) +
    tmap::tm_polygons(col = "risk", id = "RegionName", title = paste("Risk (%) of exposure to\nCOVID-19 in a group of", G), border.col = "lightgrey", border.alpha = 0.2, lwd = boundaryweights, palette = pal(bins), breaks = bins) +
    tmap::tm_shape(World) +
    tmap::tm_layout(legend.outside = TRUE, legend.outside.position = "right", title = maptitle) +
    tmap::tm_borders("grey", lwd = .5)
}


#' PerCapitaMap_tmap
#'
#' @description Creates a tmap map displaying active cases per 'people' for the input data.
#'
#' @param DATA Data containing prevalence information to map.
#' @param people Transform from proportion of population to per 'people'.
#' @param boundaryweights Weight assigned to the maps boundary edges.
#' @param projectionCRS Type of geographic projection to use.
#' @param maptitle Adds title to map.
#'
#' @return Outputs a tmap displaying active cases per 'people' for the input data.
#'
#' @family mapplots
#' @examples
#' \dontrun{
#' Denmark <- LoadData("LoadDenmark")
#' PerCapitaMap_tmap(Denmark)
#' }
#' @export
PerCapitaMap_tmap <- function(DATA, people = 100000, boundaryweights = 0.05, projectionCRS = "+proj=eqearth", maptitle = NA) { # DATA - map data, people - transform from proportion of population to per 'people', boundaryweights - polygon edge weights, projectionCRS - type of geographic projection to use, maptitle - adds a title to the map

  rlang::check_installed("tmap", reason = "to use PerCapitaMap_tmap()")
  World <- NULL
  utils::data("World", package = "tmap", envir = environment())
  # use equal earth projection
  DATA <- sf::st_transform(DATA, crs = projectionCRS)
  bins <- 0.01 * c(0, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 100) * people

  DATA$percapcases <- DATA$pInf * people

  tmap::tm_shape(DATA) +
    tmap::tm_polygons(col = "percapcases", id = "geoid", title = paste("Active cases per", prettyNum(people, big.mark = ",", scientific = FALSE), "people"), border.col = "lightgrey", border.alpha = 0.2, lwd = boundaryweights, breaks = bins) +
    tmap::tm_shape(World) +
    tmap::tm_layout(legend.outside = TRUE, legend.outside.position = "right", title = maptitle) +
    tmap::tm_borders("grey", lwd = .5)
}
