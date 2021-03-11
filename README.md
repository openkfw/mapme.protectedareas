# MAPME Protected Areas
This file contains information on the general goals of the project. For details on the repository, open-source code and information on how to contribute please have a look at our contribution description (currently in contributions.md). If you want to use the source code of this repository to create your own analysis, please have a look at our documentation pages (currently in contributions.md). If you are interested in the results of our portfolio analysis and impact evaluation please visit our reporting sites (insert link here later). 

## Goal of this repository and project

Protected Areas (short PAs) are essential to conserve endangered natural habitats with high biodiversity and ecosystem functions. KfW assists other 
countries with financial cooperation projects to establish new PAs and increase conservation efficiency in existing PA networks worldwide. 

The overarching goal of this repository is to create reproducible processing and analysis routines with open source tools such as R 
(and/or other languages, if necessary) that can serve as a software stack to enables us (and third parties) to leverage the potential of open (geo-) data to assist in planning monitoring and evaluation of projects to support PAs worldwide. 

Variables to be processed and analyzed can be grouped into five broad categories:

* general information regarding the legal PA status and administration
* ecosystems
* ecosystem services
* socio-economic variables
* threats to conservation

We rely heavily on external pre-processed datasources to derive these variables. Furthermore, our analysis is based on the IUCN/WDPA database, i.e. KfW portfolio was matched with WDPA to identify the relevant areas. 

## Expected outputs 

As such this repository has two broader spheres of action
1. Create reproducible pre-processing routines to derive the relevant geodatabase(s) for analysis
2. Create reproducible analysis routines consisting in a) a simple portfolio analysis and b) a quasi-experimental impact evaluations (IE) for the KfW portfolio. 

The specific outputs that should be produced by this repository regarding the geodatabase(s) consists in:
1. **A database for the portfolio analysis (vector data)**: This database will use PAs as units of observations. The output is expected to be a Geopackage (gpkg) with PAs as rows and all processed variables for the portfolio analysis as columns. 
2. **A database for the quasi-experimental IE (raster data)**: this database will create rasterstacks for all relevant variables regarding the IE. We need raster stacks for this exercise in order to be able to analyze also what is happening outside of protected areas. 

The specific outputs that should be produced by this repository regarding the analysis consist:

1. A portolio analysis based on Rmarkdown language to report on the portfolio and enable others to interactively explore the portfolio. 
2. An IE analysis that quantifies the treatment effects of KfW support to reduce forest cover loss in terrestrial and marine (Mangroves) ecosystems.



