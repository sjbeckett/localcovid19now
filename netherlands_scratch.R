# data from https://service.pdok.nl/kadaster/bestuurlijkegrenzen/atom/v1_0/bestuurlijkegrenzen_gml.xml

temp <- tempfile()
download.file("https://service.pdok.nl/kadaster/bestuurlijkegrenzen/atom/v1_0/downloads/bestuurlijkegrenzen_gml.zip", temp)
unzip(temp, c("Gemeentegrenzen.gml", "Gemeentegrenzen.xsd"))
st_read("Gemeentegrenzen.gml")
unlink(temp)

plot(netherlands2,max.plot=1)

netherlands2 <- st_read("..\\bestuurlijkegrenzen_gml\\Gemeentegrenzen.gml")
netherlands3 <- st_read("netherlands\\bestuurlijkegrenzen_gml\\Landsgrens.gml")
netherlands4 <- st_read("netherlands\\bestuurlijkegrenzen_gml\\Provinciegrenzen.gml")

netherlands_current <- st_read

st_read("https://geodata.nationaalgeoregister.nl/bevolkingskernen2011/wms?request=GetCapabilities&service=wms")

ggplot(data=netherlands_current)+
  geom_sf()
ggplot(data = netherlands2)+
  geom_sf()

netherlands2

st_crs(netherlands_current)

gemeentenaam_data <- netherlands_current%>%
  st_drop_geometry()%>%
  select(
    Gemeentenaam,
    Gemeentecode,
    Gemeentenummer,
    Provincie,
    Provinciecode,
    Provincienummer
  )

netherlands2%>%
  st_transform(4326)%>%
  st_write("netherlands\\geomNetherlands.geojson", )


netherlands_dat <- st_read("C:/Users/Freyja/OneDrive - Georgia Institute of Technology/CDC GRA/cdc-gra/netherlands/Netherlands/countries/data/geom/geomNetherlands.geojson")

netherlands_dat%>%
  str()
data%>%str


test_url <- "http://geodata.nationaalgeoregister.nl/bevolkingskernen2011/extract/bevolkingskernen2011.zip"

tempdir()
tempfile()
download.file(url = test_url, destfile = paste(tempdir(),"bevolkingskernen2011.zip",sep="\\"))

unzip(paste(tempdir(),"bevolkingskernen2011.zip",sep="\\"))

bevolk <- st_read("bevolkingskern_2011.shp")
head(bevolk)

ggplot(data=bevolk)+
  geom_sf()

base_url <- "https://opendata.cbs.nl/ODataApi/odata/37259eng"

GET(url=base_url)

url1 <- parse_url(base_url)
url$query <- list(
  Periods = '2019',
  
)

pop2019 <- cbs_get_data(
  id = "37259eng",
  Periods = "2019JJ00",
  CPI = "000000",
  Regions = has_substring("GM"),
  Sex = "T001038"
             )
pop2019m <- pop2019%>%
  select(Regions, Population = PopulationOn1January_1)
