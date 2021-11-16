library(tidyverse)
library(readxl)
library(sf)

temp <- tempfile()
download.file("https://service.pdok.nl/kadaster/bestuurlijkegrenzen/atom/v1_0/downloads/bestuurlijkegrenzen_gml.zip", temp)
temppath <- unzip(temp, exdir = tempdir())
netherlandsNew <- st_read(temppath[1]) # I know that "Gemeentegrenzen.gml" is the first item in temppath
unlink(temp)

netherlandsOrig <- st_read("orig/countries/data/geom/geomNetherlands.geojson")

# I entered this project in fall 2021 when we had population data that came with the original geojson. Instead of getting this data from the confusing netherlands state statistical website, i just reused it.
old_details <- netherlandsOrig%>%
  st_drop_geometry()

merged <- netherlandsNew%>%
  select(-gml_id, -Gemeentenaam)%>%
  mutate(
    Gemeentecode = paste0("GM",Code)
  )%>%
  left_join(old_details, by = "Gemeentecode")

# The statistical site (cbs.nl) publishes a spreadsheet with all the inter-municipality land and population changes summarized. 
temp2 <- tempfile()
download.file("https://www.cbs.nl/-/media/_excel/2021/22/grenswijzigingen-tussen-resp-opheffing-samenvoeging-en-nieuwvorming-van-gemeenten-1-januari-2021.xlsx",temp2, mode="wb")

changes2021 <- read_xlsx(temp2,skip=7)
unlink(temp2)

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

# Isolate areas with populations additions and subtractions

pop_change <- changes2021%>%
  filter(
    effect %in% c("to","from")
  )%>%
  select(-woningen, -area_km2)%>%
  mutate(effect = case_when(
    effect == "from" ~ -1,
    effect == "to" ~ 1
  ))%>%
  group_by(code)%>%
  summarise(netchange = sum(effect*inwoners))%>% # in case somewhere loses some population and gains other population from different sources, this gives the net change for the year
  ungroup()

# automated version because I realized how to do it
merge_update <- merged%>%
  left_join(pop_change, by = c("Code"="code"))%>%
  replace_na(list(netchange = 0))%>%
  mutate(
    Bevolkingsaantal = as.numeric(Bevolkingsaantal)+netchange
  )%>%
  select(-netchange)

# show the differences
# compdf <- compareDF::compare_df(st_drop_geometry(merged), st_drop_geometry(merge_update), group_col = "Code")

orig_border <- st_union(netherlandsOrig)

geomNetherlands <- merge_update%>%
  st_transform(st_crs(netherlandsOrig))%>%
  st_intersection(orig_border)
st_write(geomNetherlands, "./countries/data/geom/geomNetherlands.geojson", delete_dsn=TRUE)
## delete_dsn = T enables overwrite