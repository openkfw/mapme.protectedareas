# carbon-flux.R
# Authors: Om Bhandari, Johannes Schielein
# Purpose: This script contains function to create Query to the Net Forest Carbon Flux datasets from Global Forest Watch (GFW) and download the raster...
# ...of the desired grid coordinates
library("terra")

# (1) get_net_carbon_flux
# provide Latitude and Longitude as arguments for the required grid
# returns forest carbon net flux's raster file for selected grid

get_net_carbon_flux <- function(g) {
  
  tryCatch(
    {
      
      # create url
      url <- paste0("https://data-api.globalforestwatch.org/dataset/gfw_forest_carbon_net_flux/v20210331/download/geotiff?grid=10/40000&tile_id=",g,"&pixel_meaning=Mg_CO2e_ha")
      # create string for temporary file
      destfile <- paste0(tempdir(),g,".tif")
      # download the file and save it to tempfolder
      download.file(url, destfile)
      # load in temporary raster and return it (result from last line is always returned in function in raster)
      return(rast(paste0(tempdir(),g,".tif")))
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# # example
# myRaster <- get_net_carbon_flux("00N_040W")
