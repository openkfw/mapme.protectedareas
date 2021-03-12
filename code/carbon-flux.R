# carbon-flux.R
# Authors: Om Bhandari, Johannes Schielein
# Purpose: This script contains function to create Query to the Net Forest Carbon Flux datasets from Global Forest Watch (GFW)
library(raster)

# (1) get_net_flux_carbon
# provide Latitude and Longitude as arguments for the required grid
# returns forest carbon net flux's raster file for selected grid

get_net_carbon_flux <- function(lat, lon) {
  # get tempir
  mytempdir<-tempdir()
  # create url
  url <- paste0("https://gfw-data-lake.s3.amazonaws.com/gfw_forest_carbon_net_flux/v20210209/raw/per_hectare/",lat,"_",lon,"_net_flux_Mg_CO2e_ha_biomass_soil_forest_extent_2001_19.tif")
  # create string for temporary file
  destfile <- paste0(mytempdir,"/carbon_flux_",lat,"_",lon,".tif")
  # download the file and save it to tempfolder
  download.file(url, destfile)
  # load in temporary raster and return it (result from last line is always returned in function in raster)
  # raster(paste0(tempdir(),"/carbon_flux_",lat,"_",lon,".tif"))
  return(raster(paste0(mytempdir,"/carbon_flux_",lat,"_",lon,".tif")))
}

# example
# get_net_carbon_flux("20S", "060W")
