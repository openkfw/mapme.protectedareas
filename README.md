# MAPME Protected Areas

Repository for creating reproducible geodata processing routines to analyze protected areas portfolio from KfW. To find out more detailed information on this project please visit this [about page][].  

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

	.
	├── .gitignore # specifies which files to ignore from the git
	├── .Rprofile # contains information on which libraries and settings should be used at start
	├── _workflowr.yml  # yml file needed to control workflowr
	├──  analysis/
		├── about.Rmd
		├── index.Rmd
		├── license.Rmd
		├──  *.Rmd # Rmarkdown files containing the description of how individual variables where processed. 
		└── _site.yml
	├── code/  
		├── *.R # here are all the processing functions whose usage is described in the analysis part. Libraries are also loaded here
	├── data/   # raw imputable sample data for the routine. 
	├── docs/  # htmls builds of the Rmds in analysis/ 
	├── output/  # intermediate and final data output from sample data
	├── README.md
	└── renv/  # renv directory to lock used package versions

Please not the following: This repository only contains sample data to enable others to create a reproducible workflow for their data. It does not contain the whole input and output data from very large datasets which are used to process the whole KfW PA portfolio and create the analysis. 

# Issues and their labels (priorization)
This repository offers common labels such as `bug` or `enhancement`. In addition it also has labels to create a priorization scheme to see which issues should be prioritized and adressed first. To that end we use a simplified method called *MoSCoW* which categorizes tasks into *Must*, *Should*, *Could* and *Won't*. Issues of category *Must* are the most relevant and should be adressed first. After adressing all of these issues we will move forward to the *Should* and if time left to the *Could*. In addition there is a label called *Fast Lane* which is used to mark such issues that should be adressed first **within** their given category. So an issue with *Should* and *Fast Lane* should be adressed quickly after all of the *musts* are processed. 


[workflowr]: https://github.com/jdblischak/workflowr
[renv]:https://rstudio.github.io/renv/index.html
[renv]:/about.html
