LoadGhana <- function(){
  
  geomGhana <- st_read("countries/data/temp_geom/geomGhana.geojson")
  
  temp <- tempfile()
  download.file(url="https://www.dropbox.com/s/2uxzix4upet0nlm/cases_ghana.csv?dl=1", destfile = temp)
  
  casesGhana <- vroom(temp)
  
  case_data <- casesGhana%>%
    group_by(name)
    
  
}

geomRaw <- st_read("C:\\Users\\Freyja\\Downloads\\Regions\\Map_of_Regions_in_Ghana.shp")

geomDistrictsRaw <- st_read("C:\\Users\\Freyja\\Downloads\\Districts\\Map_of_Districts_216.shp")



geomRegions <- geomRaw%>%
  st_drop_geometry()%>%
  mutate(
    name = str_to_title(REGION),
    name = str_replace(name, "Brong Ahafo", "Brong-Ahafo")
  )

geomDistricts <- geomDistrictsRaw%>%
  st_drop_geometry()%>%
  select(DISTRICT)%>%
  mutate(
    name = str_to_title(DISTRICT)
  )%>%
  arrange(name)

ghanaRegions <- casesGhana%>%
  select(name)%>%
  distinct()%>%
  separate(
    col = name,
    into = c("name","geography"),
    sep = "\\s(?=Region)")%>%
  full_join(
    geomRegions, by="name"
  )%>%
  left_join(geomDistricts, by = "name")

ghanaRegions <- st_read("https://services1.arcgis.com/93Ieg4hfTR8LTwJ7/ArcGIS/rest/services/new_regions/FeatureServer/0/query?where=%22FID%22+LIKE+%27%25%27&objectIds=&time=&geometry=&geometryType=esriGeometryPolygon&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=standard&f=pgeojson&token=")


# st_write(ghanaRegions, "countries/data/orig_geom/geomGhana.geojson")


ghanaRegions$macro_code = c("SV","BO", "AF", "BE", "AA", "OT", "WP", "WN", "UW", "UE", "EP", "CP", "AH", "NE", "NP", "TV")

geomGhana <- ghanaRegions%>%
  select(
    macro_code,
    macro_name = REGION
  )%>%
  st_make_valid()
  

st_write(geomGhana, "countries/data/temp_geom/geomGhana.geojson")
