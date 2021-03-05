# MAPME Protected Areas

Repository for creating reproducible geodata processing routines to analyze protected areas portfolio from KfW 

Variables will cover: 
* general PA and administrational information
* ecosystems
* ecosystem sercvices
* socio-economic variables
* threats to conservation

# Repository structure and reproducibility
This repository is structured as a reproducible project using the workflowr package. For more information please visit [workflowr][] website. In addition, this repository used the the renv package to enable others to exactly work with the same package versions that had been utilized in this project. For more information please visit the [renv][] website.  

The folder structure is as follows
mapme.protectedareas/

├── .gitignore # specifies which files to ignore from the git

├── .Rprofile # contains information on which libraries and settings should be used at start

├── _workflowr.yml  # yml file needed to control workflowr

├── analysis/     

│   ├── about.Rmd

│   ├── index.Rmd

│   ├── license.Rmd

│   ├── *.Rmd # here are all Rmd files containing the description of the data processing. 

│   └── _site.yml

├── code/  

│   ├── main.R  # main script used for the primary preprocessing

│   ├── setup.R  # a setup script loading libs and setting up an environment

│   ├── *.R # here are all the processing functions whose usage is described in the analysis part

├── data/   # raw imputable sample data for the routine. 

├── docs/  # htmls builds of the Rmds in analysis/ 

├── output/  # intermediate and final data output from sample data

├── README.md

└── renv/  # renv directory to lock used package versions


Please not the following: This repository only contains sample data to enable others to create a reproducible workflow for their data. It does not contain the whole input and output data from very large datasets which are used to process the whole KfW PA portfolio and create the analysis. 


[workflowr]: https://github.com/jdblischak/workflowr
[renv]:https://rstudio.github.io/renv/index.html