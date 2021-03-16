# MAPME Protected Areas - How to contribute
This file includes information on how to use this repository and create good contributions. We also use a code of conduct for open source projects adapted from the Contributor Covenant homepage. You can find this conduct in the file CoC.md. 

## How can I use this repository?
Put here information on the usage (later). 

## How can I contribute? 
1. [Reporting Bugs & Suggesting Enhancements](#reporting-bugs-and-suggesting-enhancements)
2. [Contributing Code](#remarks-on-contributing-code)
3. [Issues and Prioritization](#issues-and-prioritization)

### Reporting Bugs And Suggesting Enhancements
Add here information on where and how to report bugs and add suggestions (later)

### Remarks on Contributing Code
In order to ensure reproducibility, proper documentation and open-source code is key. In addition to making all routines public, we also have a strong
focus on the proper **documentation of utilized methods**. Here are the necessary steps contributors should do to make a contribution to this repository. 

1. **Read this contrib file**. It contains most of the relevant information to get started. 
2. **Create a new issue** in this repository that describes your intended contribution and wait for a reaction from the community.
3. If you are a contributor to this repository **open a new branch** to work on your issue. Give the branch a name that makes it easy to understand what the intended contribution might be. **Example for Branchname**: *wwf_teow_preprocessing* to add preprocessing routines for the WWF/TEOW dataset.  
4. If you are not a contributor (yet) you might want to forge this repository first and later create an upstream pull request. 
5. This repository uses the [tidyverse Styleguide][] for creating commits in R. Please have a look at it, if you are not familiar.
6. This repository tries to follow the recommendations of [Ropenscilabs][] for developing spatial software in R. Please have a look at it, if you are not familiar. **The two main libraries to be used for geo-processing are `sf` and `terra`. Please try to write your functions using these packages instead of the older packages `sp` and `raster`.**
7. Before creating new code you might want to have a **look at the existing code-base** to see whether you can take advantage of existing processing or analysis routines. The easiest way is probably to have a look at the documentation pages (<-insert link from index later here->) and look at the utilized routines there before going into the code itself. It is encouraged re-utilize code from the *code* folder as much as possible to reduce maintenance needs and complexity. 
8. We use `R` scripts to process our data and `Rmarkdownscripts (Rmd)` to document the utilized data, methods, pre-processing routines and analysis scripts. We have some **basic recommendations and minimum standards for code developemt** which are shown below.
9. Develop your code with  **sample data** that is quick to process and easy to reproduce. There are also some recommendations on sample data below.

[tidyverse Styleguide]: https://style.tidyverse.org/
[Ropenscilabs]: https://ropenscilabs.github.io/statistical-software-review-book/standards.html#spatial-software


### Repository Structure
This repository is structured as a reproducible project using the workflowr package. For more information please visit [workflowr][] website. In addition, this repository used the the renv package to enable others to exactly work with the same package versions that had been utilized in this project. For more information please visit the [renv][] website.  

mapme.protectedareas/

	.
	├── .gitignore # specifies which files to ignore from the git
	├── .Rprofile # contains information on which libraries and settings should be used at start
	├── _workflowr.yml  # yml file needed to control workflowr
	├──  analysis/
		├── about.Rmd
		├── index.Rmd
		├── license.Rmd 
		├──  *.Rmd # Rmarkdown files to document pre-processing and analysis routines
		└── _site.yml
	├── code/  
		├── *.R # R-functions and scripts utilized in Rmd
	├── data/   # raw imputable sample data for the routine. 
	├── docs/  # htmls builds of the Rmds in analysis/ 
	├── output/  # intermediate and final data output from sample data
	├── README.md # general information on the project
	└── renv/  # renv directory to lock used package versions

[workflowr]: https://github.com/jdblischak/workflowr
[renv]:https://rstudio.github.io/renv/index.html

### Preprocessing Datasets
This is a step-by-step guide to create a new pre-processing routine based on the example of TEOW ecoregions dataset from WWF. **Pre-processing routines are organized in this repository along the line of thematic data-sets from differing data-sources**. The intention of this organization is to have a **modular structure** that allows for easy adding or deleting data-sources from the pre-processing routine and eventually chain them together. Also this allows us to debug the code more easily if the routine is chained. **An exception** to this  structure are routines that allow access to already pre-processed data-sets, in our case the `API`-Access *DOPA/JRC Rest Services*, which provides tabular information for several thematic data-sets pre-processed by JRC on the base of PAs.

Please try to develop your routine using a small subset of **sample** data that takes few processing time for the documentation and has low storage requirements. For PAs we recommend e.g. to use 1 or 2 PAs from your area of interest. We currently use the `wdpar` package to automatically download and preprocess sample PAs from the *WDPA*. You can also save the sample data in the *data* folder. This is recommended if the dataset you use has no definitive storage place on the internet. Nevertheless, it is encouraged to include the downloading process in the routine as well and save the sample data in the temporary folder that is created by the *Rsession*. 

**To create a new pre-processing routine you should**

1. Create a new issue for the data-source that should be processed. The Title could be for example: *"Pre-process TEOW ecoregion data from WWF to create tabular data for PA polygon database"*
2. Create a new branch in the repository as discussed above. This branch should indicate what is done e.g. *"preprocess_teow"*
3. Have a look at existing processing routines as mentioned above to see if you can recycle code or use existing routines. 
4. Create a `Rmd` file placed in the *analysis* folder (see structure above). Best practice is to use the `wflow_open()` command which will create a new `Rmd` file. Name this new file according to the pre-processed data-source and variable e.g. `wwf_teow.Rmd` for TEOW Ecoregions from the World Wildlife Fund for Nature (WWF) or `gfw_forests.Rmd` for different variables from the Global Forest Watch (GFW) such as *forest cover*, *forest cover loss* or *emissions from forest cover loss*. The full command to create a new script in our example would therefore look like this `wflow_open("analysis/wwf_teow.Rmd")`
5. Create a `R`-script for larger functional code-junks in the *code* folder (if you need to create new routines). This file can be sourced in the `Rmd` (details and naming convention below).
6. Create a reference to the new routine and its documentation by creating a link to the rendered html file in `index.Rmd` (details below)
7. Add relevant meta-data to the `Rmd` file (details below).

#### Seperate R scripts with source code 
You should try to develop R-functions that are seperated in an R-script and then sourced in the `Rmd` files. Those functions and R-scripts should be placed in the *code* folder (see structure above). This is especially relevant for code which can be re-used in several pre-processing routines such as chained pre-processing steps e.g. `reproject` -> `rasterize` -> `stack` -> `zonal`. 

Here you have two differing naming conventions when saving the script:

* You can create a new name that links the `R` file to the name of your `Rmd`file. This could be for example *preprocessing_wwf_teow.R*. Use this if your code is most probably specific to the dataset that you process. 
* If you create a new processing routine that is more generic and can be utilized in several other projects as well. Try to find a name that best describes this routine. For the example chain above such name could be *rasterize_and_zonalstats.R*. If you create such a generic routine make sure to also create a link and a small descrpition of this new routine in the `index.Rmd` file as described in the subsequent section. 

#### Render and link your report
All of the files in the *analysis* folder will be rendered to create the project website. In order to render new `Rmd` files use the function `wflow_build()` from the `workflowr` package. After rendering, html files will be created in the *docs* folder. **PLEASE MAKE SURE to create new links in the `index.Rmd` that will reference to this new html files**. This will ensure, that the new routines appear in the rendered website afterwards. 

**PLEASE MAKE also sure to document newly created `R` files in `index.Rmd`** to allow others to get a quick assessment of the existing pre-processing routines. You only need to do this if you think your routine could be used also outside of the context in which you processed your dataset (i.e. more generic methods as shown above in the example of chained processing steps). If possible you can also use a graphical expression of the workflow (see examples in index) which will enable others to understand your routine quickly. Images for such graphical expressions should be stored in *docs* as well containing the name of the routine. For the example above this would be *rasterize_and_zonalstats.png*. You can find a Powerpoint Template for workflows in the *docs* folder as well. 

#### Contents of the `Rmd` file
In order to create a good documentation of the processed data and the authors of the script we would like to ask for some minimal information in the `Rmd` files consisting of

* Author Name (and optionally contact details, link to github account or other)
* Purpose of the script
  * what is shown by the dataset(s) and what information is to be derived, 
  * what is done in the script and which processing scripts from the *code* folder are used
  * what data comes out in the end. 
* Meta-data for the processed dataset(s)
  * Name of the dataset(s) and source(s) (scientific citation if available)
  * Version number of the data (if the data is updated and possess this information)
  * geographical extension
  * spatial resolution
  * temporal resolution
  * link to the main meta-data document from the data-source
  * download-link
  * when was the data downloaded
* Detailed description of the data processing
* Time necessary to process the sample data and details on the machine used to process it. 


### Analysis
This will be added later. 

## Issues and Prioritization
This repository offers common labels such as `bug` or `enhancement`. In addition it also has labels to create a priorization scheme to see which issues should be prioritized and adressed first. To that end we use a simplified method called *MoSCoW* which categorizes tasks into `Must`, `Should`, `Could` and `Won't`. Issues of category `Must` are the most relevant and should be addressed first. After addressing all of these issues we will move forward to the `Should` and if time left to the `Could`. In addition there is a label called `Fast Lane` which is used to mark such issues that should be addressed first **within** their given category. So an issue with `Should` and `Fast Lane` should be adressed quickly after all of the `Musts` are processed. 

