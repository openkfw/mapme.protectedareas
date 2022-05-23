# MAPME Protected Areas
This file contains information on the general goals of the project. For details on the repository, open-source code and information on how to contribute please have a look at our contribution description (currently in contributions.md). If you want to use the source code of this repository to create your own analysis, please have a look at our documentation pages (currently in contributions.md). If you are interested in the results of our portfolio analysis and impact evaluation please visit our [reporting sites](https://openkfw.github.io/mapme.protectedareas/index.html) which are work in progress and might change constantly.  

## Goal of this repository and project

Protected Areas (short PAs) are essential to conserve endangered natural habitats with high biodiversity and ecosystem functions. KfW assists other countries with financial cooperation projects to establish new PAs and increase conservation efficiency in existing PA networks worldwide. 

The overarching goal of this repository is to create reproducible processing and analysis routines with R that can serve as a software stack to enables us (and third parties) to leverage the potential of open (geo-) data to assist in planning monitoring and evaluation of projects to support PAs worldwide. 

Variables to be processed and analyzed can be grouped into five broad categories:

* general information regarding the legal PA status and administration
* ecosystems
* ecosystem services
* socio-economic variables
* threats to conservation

We rely heavily on external pre-processed datasources to derive these variables. Furthermore, our analysis is based on the IUCN/WDPA database, i.e. KfW portfolio was matched with WDPA to identify the relevant areas. 

## Expected outputs 

The tree main goals of this project are: 

1. Create reproducible data pre-processing routines to derive the relevant geodatabase(s) for analysis. Most of our routines are now implemented in the [mapme.biodiversity package](https://github.com/mapme-initiative/mapme.biodiversity) for R. 
2. Create reproducible analysis workflows that process the data for our portfolio and can help to stimulate the discussion on how to further use this data
3. Create a reproducible spatial impact evaluation workflow to estimate conservation effects in supported protected areas. 

The specific outputs that should be produced are:

1. **A database on the portfolio level (vector data)**: This database will use PAs as units of observations.  
2. **A gridded database on the portfolio level (vector data)**: This database will use gridcells with a size of 5 hectars as the unit of observations. Processed areas can cover supported PAs, non-supported PAs and non-protected areas. 
3. **The analysis scripts and processing** routines for reproducibility and transparency.
4. **[Interactive data products](https://openkfw.github.io/mapme.protectedareas/index.html)** that show the results our analysis. 


