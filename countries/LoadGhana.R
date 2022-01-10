LoadGhana <- function(){
  
  geomGhana <- st_read("countries/data/temp_geom/geomGhana.geojson")
  
  temp <- tempfile()
  download.file(url="https://www.dropbox.com/s/2uxzix4upet0nlm/cases_ghana.csv?dl=1", destfile = temp)
  
  casesGhana <- vroom(temp)
  
  case_data <- casesGhana%>%
    group_by(name)%>%
    select(-starts_with("iso"),-cases)%>%
    mutate(
      latest_date = max(date),
      past_date = max(date<=(latest_date-14))
    )%>%
    filter(date==latest_date | date == past_date)%>%
    summarise(
      datadate,
      case_diff = 
    )
    
  
}
