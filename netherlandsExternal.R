# This has been replaced by netherlandsAdjustPop.R
library(cbsodataR)
library(sf)

temp <- tempfile()
tempd <- tempdir()
download.file("https://service.pdok.nl/kadaster/bestuurlijkegrenzen/atom/v1_0/downloads/bestuurlijkegrenzen_gml.zip", temp)
temppath <- unzip(temp, exdir = tempdir())
geomNetherlandsR <- st_read(paste(tempd,"Gemeentegrenzen.gml",sep="\\"))
unlink(temp)
unlink(tempd)

st_write(geomNetherlands, "geomNetherlands.geojson")

pop2019 <- cbs_get_data(
  id = "37259eng",
  Periods = "2019JJ00",
  CPI = "000000",
  Regions = has_substring("GM"),
  Sex = "T001038"
)
pop2019 <- pop2019[,c("Regions", "PopulationOn1January_1", "Periods")]
names(pop2019) <- c("Code","Bevolkingsaantal", "Year")
pop2019$Year <- gsub(pattern = "JJ00", replacement = "",pop2019$Year)

write.csv(
  pop2019,
  "netherlandsMuniPop.csv"
)
