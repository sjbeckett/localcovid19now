# subregionalcovid19

This repository serves as a framework to assemble and visualize recent case data from regional data sources across the globe. The package was assembled by researchers at the Georgia Institute for Technology and the Applied Bioinformatics Laboratory, and forms a core component of exposure risk calculations used in the [COVID-19 Event Risk Tool](https://covid19risk.biosci.gatech.edu/).

We compile data from a variety of [sources](https://github.com/sjbeckett/subregionalcovid19/blob/main/DataSources.md).

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
```
