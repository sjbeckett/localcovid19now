---
title: 'localcovid19now: processing and mapping COVID-19 case data at subnational scales'
tags:
  - R
  - epidemiology
  - COVID-19
  - mapping
authors:
  - name: Stephen J. Beckett^[Corresponding author]
    orcid: 0000-0002-4410-2960 
    affiliation: "1"
  - name: Freyja A. Brandel-Tanis
    orcid: 0000-0003-2870-6980
    affiliation: "2, 3"
  - name: Quan Nguyen
    affiliation: "1, 4"
  - name: Aroon T. Chande
    orcid: 0000-0001-5260-3135
    affiliation: "5"
  - name: Lavanya Rishishwar
    orcid: 0000-0002-2055-9392
    affiliation: "1, 5"
  - name: Clio Andris
    orcid: 0000-0002-8559-5079 
    affiliation: "2, 6"
  - name: Joshua S. Weitz
    orcid: 0000-0002-3433-8312
    affiliation: "1, 7, 8"
affiliations:
 - name: School of Biological Sciences, Georgia Institute of Technology, Atlanta, GA, United States of America
   index: 1
 - name: School of City and Regional Planning, Georgia Institute of Technology, Atlanta, GA, United States of America
   index: 2
 - name: School of Civil and Environmental Engineering, Georgia Institute of Technology, Atlanta, GA, United States of America
   index: 3
 - name: School of Chemical and Biomolecular Engineering, Georgia Institute of Technology, Atlanta, GA, United States of America
   index: 4
 - name: Applied Bioinformatics Laboratory, Georgia Institute of Technology, Atlanta, GA, United States of America
   index: 5
 - name: School of Interactive Computing, Georgia Institute of Technology, Atlanta, GA, United States of America
   index: 6
 - name: School of Physics, Georgia Institute of Technology, Atlanta, GA, United States of America
   index: 7
 - name: Institut de Biologie, École Normale Supérieure, Paris, France
   index: 8
date: 21 September 2022
bibliography: refs.bib

---

# Summary

The *[localcovid19now](https://github.com/sjbeckett/localcovid19now)* R package provides functionality to load, unify and visualize recent COVID-19 case data at subnational scales in order to provide localized situational reports and improve understanding of the scale of local COVID-19 transmission. The package loads data from a variety of data sources and returns the most recent estimate of recorded per capita active COVID-19 infections, the date of the most recent report, and the geometry of each region. These data can then be visualized via mapping documented per capita active infections. We also provide functionality to visualize the risk of exposure to COVID-19, given a particular event size.

# Statement of need

The COVID-19 global pandemic has led to hundreds of millions of recorded cases, over six million recorded deaths, and critical pressures on health and economic systems [@10665-362241]. A key challenge in mitigating ongoing spread is that disease transmission and human behavior are coupled [@funk2010modelling; @bavel2020using; @weitz2020awareness]. Coupled viral and human dynamics have limited the predictive window of infectious disease models (including those of COVID-19), while also catalyzing efforts to develop awareness campaigns that leverage localized real-time information to inform the public and mitigate transmission. Beginning in May 2020, our team launched the COVID-19 Event Risk Tool (<https://covid19risk.biosci.gatech.edu/>), a situational dashboard with maps estimating exposure risk to COVID-19 across counties in the US [@chande2020real]. We use the number of active cases per capita and an assumed case ascertainment bias to estimate current local COVID-19 prevalence $p$. We then quantify and visualize COVID-19 exposure risk $\kappa$ (%) within a group of size $n$ as $\kappa = 100\times(1-(1-p)^n)$. Since launch, more than 16M individuals have accessed the site and several similar international efforts have been initiated, e.g., for [Italy](https://covid19eventi.datainterfaces.org/) [@Italy], [Spain](https://eventosycovid19.es/) [@Spain], [Mexico](https://adrian-acuna.shinyapps.io/Dashboard-MX-v3/_w_33ffcba1/#shiny-tab-Riesgo) [@Mexico], [Argentina](https://www.lanacion.com.ar/sociedad/fiestas-fin-ano-calcula-riesgo-coronavirus-infectado-nid2548498/#/) [@Argentina]. Here, in the *localcovid19now* R package, we extend our initial release [@chande2020real] to develop a pipeline to combine population estimates, COVID-19 case data, and spatial geometry such that risk estimates can be performed at localized scales across multiple countries. Our package aims to facilitate the process of downloading and visualizing recent COVID-19 data, in support of public health research and campaigns to inform and alert the public of the ongoing risk of transmission -- and is of particular interest to public health scientists and researchers, epidemiologists, community leaders, and policymakers.

Our work is complementary to existing efforts to provide localized COVID-19 data across multiple countries, e.g., [@palmer2021covidregionaldata; @dong2020interactive; @Wahltinez2020; @COVID19]. Via *localcovid19now*, we focus on aggregating recent case data rather than assembling a timeseries of all different COVID-19 data metrics. We use recent case data as the basis for providing localized real-time estimates of active cases. Additionally, spatial coverage differs among existing aggregate efforts to harmonize COVID-19 data. In order to maximize geographic coverage, the *localcovid19now* package pulls in data collated by John Hopkins U CSSE [@dong2020interactive], from Google COVID-19 Open Data [@Wahltinez2020], the Humanitarian Data Exchange, the COVID-19 Data Hub [@COVID19, @guidotti2022],  and the [WHO European Region COVID19 Subnational Explorer](https://experience.arcgis.com/experience/3a056fc8839d47969ef59949e9984a71) [@WHO]. We also include additional governmental/volunteer curated subnational datasets, e.g., [@cota2020monitoring; @jefferies2020covid; @berry2021sub], (a full list of sources is provided with the [package](https://github.com/sjbeckett/localcovid19now)). Individual sources store data in different file formats, languages and file structures requiring tailored data retrieval strategies. Given that as time progresses and needs or efforts of those maintaining COVID-19 datasets change, the sourced datasets may change formats (e.g., Czechia added an extra identifier column to their dataset in November 2021 [@Czechia]), change reporting areas (e.g., administrative border changes in the Netherlands), become deprecated (e.g., Cuba which last updated in September 2021), or otherwise be forced offline (e.g., [Ireland](https://www.bbc.com/news/world-europe-58413448) [@Ireland]). For this reason we have modularized our data reading process such that errors in one part of the pipeline do not prevent the loading of other datasets. An example of the number of active cases per 100,000 individuals as read
and visualized via the \textit{localcovid19now} pipeline is shown in \autoref{fig:global}.

![Active cases per 100,000 people (as of 9 December 2022). Greyscale polygons denote ar-
eas where data estimates are over 30 days old, which we show here to show the current package
scope. This map was created using the localcovid19now package by running the code: `GlobalMap =
LoadData(dropNAcountries=FALSE); PerCapitaMap_tmap(GlobalMap,100000)`. More examples are
shown on the packages homepage.\label{fig:global}](Global_pc_tmap.png)

 Our motivation in developing this project is to provide data-driven localized epidemic information that connects case data to potential exposure risk. While individual perception of COVID-19 exposure risk is linked to the utilization of risk-reducing actions, individual assessment of risk has been found to be generally uncorrelated to estimated exposure risk [@Sinclaire2100970118]. In contrast, communicating infection exposure risk estimates, as we do at the [COVID-19 Event Risk Tool](https://covid19risk.biosci.gatech.edu), has been shown as an effective intervention in realigning perceptions of localized event risks [@Sinclaire2100970118; @sinclair2021imagining; @sinclair2022counteracting]. The package additionally includes functionality to help users visualize data (via leaflet [@leaflet] or tmap [@tmap]), including that of per capita active documented infections and estimated event-based risk.
 
# Acknowledgements

We thank Seolha Lee for her contributions to developing the COVID-19 Event Risk Tool. We thank Kaitlyn Johnson, Robel Kassa, and Leo Wolansky for constructive feedback. Development of the *[localcovid19now](https://github.com/sjbeckett/localcovid19now)* package was supported by funds from the Centers for Disease Control and Prevention (75D30121P10600), The Rockefeller Foundation (11356), the Charities in Aid Foundation, the Marier Cunningham Foundation, and the Chaire Blaise Pascal funds of the Île-de-France region.

# Author contributions

SJB led the project. SJB, FAB, QN and ATC contributed to dataset collection and coding methods for data curation. SJB, FAB and ATC contributed to package development. LR, CA, and JSW provided substantial contributions via decision and design guidance, and project management. SJB wrote the manuscript with contributions from all authors.

# References
