remSurplus <- function(
  input_file,
  collection_files = c("geomEurope","geomSmallCountries")
) {
  isoFile <- input_file%>%
    st_drop_geometry()%>%
    select(iso3, filename)%>%
    distinct()
  
  coll1Iso <- isoFile%>%dplyr::filter(filename==collection_files[1])%>%pull(iso3)
  distIso <- isoFile%>%dplyr::filter(!filename %in% collection_files)%>%pull(iso3)
  
  input_file <- input_file%>%
    mutate(
      isDup = case_when(
        filename == collection_files[2] & iso3 %in% coll1Iso ~ 1,
        filename %in% collection_files & iso3 %in% c(distIso, "ASM", "GUM", "MNP", "VIR","PRI") ~ 1,
        TRUE ~ 0
      )
    )%>%
    dplyr::filter(isDup == 0)%>%
    select(-isDup)
  
  return(input_file)
}