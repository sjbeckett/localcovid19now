# subregionalcovid19

This repository serves as a framework to assemble and visualize recent case data from regional data sources across the globe. The package was assembled by researchers at the Georgia Institute for Technology and the Applied Bioinformatics Laboratory, and forms a core component of exposure risk calculations used in the [COVID-19 Event Risk Tool](https://covid19risk.biosci.gatech.edu/).

We compile data from a variety of [sources](https://github.com/sjbeckett/subregionalcovid19/blob/main/DataSources.md).

A description of the package can be found [here](https://github.com/sjbeckett/subregionalcovid19/tree/main/Paper/Paper.pdf).

## Citation

Please cite our work as:
> Beckett, S.J., Brandel-Tanis F., Nguyen Q., Chande A., Lee S., Rishishwar L., Andris C., Weitz J.S. subregionalcovid19: global processing and mapping infectious cases of COVID-19 at subnational scales ...

## Installation

The subregionalcovid19 library is available in R. It can be installed by running:

```R
library("devtools")
install_github("sjbeckett/subregionalcovid19")
library("subregionalcovid19")
```

## Use

Functionality is provided to load in data for a particular country (e.g. `US <- LoadUS()`) , or to load all available datasets as:
```R
GLOBALMAP <- LoadCountries()
GLOBALMAP <- tidy_Data(GLOBALMAP) #sets data older than 30 days, or with negative or zero differences in active cases to NA.
```

Once loaded, the per capita active case data can be mapped with built in helper functions: 
```R
#show per capita active cases per 100,000 people in leaflet
PerCapitaMap_leaflet(GLOBALMAP,100000)

#show per capita active cases per 100,000 people in leaflet
PerCapitaMap_tmap(GLOBALMAP,100000)
```

Additionally, with a provided assumed ascertainment bias (ratio between true infections and recorded cases), the risk that one or more people in a group of a particular size can be estimated and mapped:

```R
#assume an ascertainment bias of 4 infections per recorded case.
GLOBALMAP$AB <- 4 

#show risk that one or more people are infectious in a group of 50 using leaflet
EventMap_leaflet(GLOBALMAP,50)

#show risk that one or more people are infectious in a group of 100 using tmap
EventMap_tmap(GLOBALMAP,100)

#maps can be saved using tmap commands:
MAP = EventMap_tmap(GLOBALMAP,100)
tmap_save(MAP,"GlobalRiskMap.png")

#we can also create a table of event risk by location (first dropping geometry) as:
GLOBALDAT <- sf::st_drop_geometry(GLOBALMAP) %>%
    dplyr::as_tibble()
create_c19r_data(df_in = GLOBALDAT)
```


