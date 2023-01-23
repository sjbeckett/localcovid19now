# localcovid19now

This repository serves as a framework to assemble and visualize recent case data from regional data sources across the globe. The package was assembled by researchers at the Georgia Institute for Technology and the Applied Bioinformatics Laboratory, and forms a core component of exposure risk calculations used in the [COVID-19 Event Risk Tool](https://covid19risk.biosci.gatech.edu/).

We compile data from a variety of [sources](https://github.com/sjbeckett/localcovid19now/blob/main/DataSources.md).

A description of the package can be found [here](https://github.com/sjbeckett/localcovid19now/tree/main/paper) or in our [paper](https://doi.org/10.21105/joss.04898).

[![DOI](https://joss.theoj.org/papers/10.21105/joss.04898/status.svg)](https://doi.org/10.21105/joss.04898)


## Citation

Please cite our work as:
> Beckett, S.J., Brandel-Tanis F.A., Nguyen Q., Chande A.T., Rishishwar L., Andris C., Weitz J.S. 2023. localcovid19now: processing and mapping COVID-19 case data at subnational scales. Journal of Open Source Software, 8(81), 4898, https://doi.org/10.21105/joss.04898

## Installation

The localcovid19now library is available in R. It can be installed by running:

```R
library("devtools")
install_github("sjbeckett/localcovid19now")

library("localcovid19now")
```
We note that our package uses other common packages across the R ecosystem to run. The installation of these packages should be initialized by the above commands. We also note that some users [may need to change default timeout settings](https://github.com/sjbeckett/localcovid19now/issues/58) in order to install the package.

## Use

Functionality is provided to load in data for a particular country, for example:
```R
US <- LoadData("LoadUS")

Malaysia <- LoadData("LoadMalaysia")

US_and_Malaysia <- LoadData(c("LoadUS","LoadMalaysia"))

#Note that to load data for the Philippines the googledrive package is used, requiring users' google credentials. The permission to "See, edit, create, and delete all of your Google Drive files." needs to be checked for this to work.
#See more here: https://googledrive.tidyverse.org/reference/drive_auth.html
#credentials can be set before running the function:
googledrive::drive_auth(email = TRUE)

Philippines <- LoadData("LoadPhilippines")

#A list of loading options is given by:
countrylist

```
or to load all available datasets as:

```R
GLOBALMAP <- LoadData()  #note, by default, this applies tidying to data such that countries whose data is older than 30 days is dropped.

#can additionally see the full scope of potential datasets as:
GLOBALMAP2 <- LoadData(dropNACountry = FALSE) 
```

Once loaded, the per capita active case data can be mapped with built in helper functions: 
```R
#show per capita active cases per 100,000 people in leaflet for GLOBALMAP
PerCapitaMap_leaflet(GLOBALMAP,100000)

#show per capita active cases per 100,000 people in tmap for GLOBALMAP2
PerCapitaMap_tmap(GLOBALMAP2,100000)
```

![Active cases per capita across the globe](/inst/examples/Global_pc_tmap.png)

Additionally, with a provided assumed ascertainment bias (ratio between true infections and recorded cases), the risk that one or more people in a group of a particular size can be estimated and mapped:

```R
#assume an ascertainment bias of 4 infections per recorded case.
US$AB <- 4 

#show risk that one or more people are infectious in a group of 50 using leaflet
EventMap_leaflet(US,50,US$AB)

#show risk that one or more people are infectious in a group of 100 using tmap
EventMap_tmap(US,100,US$AB)

#maps can be saved using tmap commands. Here we also use a projection more suited to the US:
MAP = EventMap_tmap(US,100,US$AB,projection=5070)
tmap::tmap_save(MAP,"US_RiskMap.png")

#Some resources for choosing projections can be found here: https://proj.org/usage/index.html and here: https://epsg.io/ and 

#we can also create a table of event risk by location as:
create_c19r_data(df_in = US, asc_bias_list = cbind(AB4 = US$AB))
```

![event risk in the US](/inst/examples/US_RiskMap.png)

We caution that the case ascertainment bias may differ both across different regions and across time (due to differences in testing strategies).


## Community guidelines

We welcome contributions from the community. We encourage and recommend that feedback, bug reports, and feature requests should first be documented as an [Issue](https://github.com/sjbeckett/localcovid19now/issues/) on GitHub.
