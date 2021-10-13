library(tidyverse)
library(readxl)
library(sf)

temp <- tempfile()
download.file("https://service.pdok.nl/kadaster/bestuurlijkegrenzen/atom/v1_0/downloads/bestuurlijkegrenzen_gml.zip", temp)
unzip(temp, c("Gemeentegrenzen.gml", "Gemeentegrenzen.xsd"))
netherlandsNew <- st_read("Gemeentegrenzen.gml")
unlink(temp)

netherlandsOrig <- st_read("orig/countries/data/geom/geomNetherlands.geojson")

old_details <- netherlandsOrig%>%
  st_drop_geometry()

merged <- netherlandsNew%>%
  select(-gml_id)%>%
  mutate(
    Gemeentecode = paste0("GM",Code)
  )%>%
  left_join(old_details, by = "Gemeentecode")

changes2021 <- read_xlsx("grenswijzigingen-tussen-resp-opheffing-samenvoeging-en-nieuwvorming-van-gemeenten-1-januari-2021.xlsx",skip=7)

changes2021 <- changes2021%>%
  slice(3:15)%>%
  select(
    from = `nog bestaande gemeente...1`,
    dropped = `opgeheven gemeente`,
    to = `nog bestaande gemeente...5`,
    new = `nieuw gevormde gemeente`,
    inwoners,
    woningen,
    area_km2 = `km2 land 1`
  )%>%
  pivot_longer(
    cols = c(1:4),
    names_to = "effect",
    values_to = "region"
  )%>%
  drop_na()%>%
  separate(
    col = region,
    into = c("code","name"),
    sep = "\\s",
    extra = "merge"
  )

# Look only at those with to/from greater than 10. This was arbitrary, probably 100+ would be more appropriate, but it didn't matter here.
pop_change <- changes2021%>%
  filter(
    effect %in% c("to","from"),
    inwoners>=10
    )%>%
  select(-woningen, -area_km2)

# Because we had so few changes, I didn't try to make it more automated. It proved not worth it.
merge_update <- merged%>%
  mutate(
    Bevolkingsaantal = case_when(
      Code == pop_change$code[1] ~ as.numeric(Bevolkingsaantal)+pop_change$inwoners[1],
      Code == pop_change$code[2] ~ as.numeric(Bevolkingsaantal)+pop_change$inwoners[2],
      Code == pop_change$code[3] ~ as.numeric(Bevolkingsaantal)+pop_change$inwoners[3],
      Code == pop_change$code[4] ~ as.numeric(Bevolkingsaantal)+pop_change$inwoners[4],
      TRUE ~ as.numeric(Bevolkingsaantal)
    )
  )
# show the differences
#compareDF::compare_df(st_drop_geometry(merged), st_drop_geometry(merge_update), group_col = "Code")

geomNetherlands <- merge_update%>%
  st_transform(st_crs(netherlandsOrig))
st_write(geomNetherlands, "./countries/data/geom/geomNetherlands.geojson", delete_dsn=TRUE)
# delete_dsn = T enables overwrite