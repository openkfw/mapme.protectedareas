# carbon-flux.R
# Authors: Om Bhandari, Johannes Schielein
# Purpose: This script contains function to create Query to the Net Forest Carbon Flux datasets from Global Forest Watch (GFW)


# (1) get_net_flux_carbon
# provide Latitude and Longitude as arguments for the required grid
# returns forest carbon net flux's raster file for selected grid

get_net_carbon_flux <- function(lat, lon) {
  # create url
  url <- paste0("https://gfw-data-lake.s3.amazonaws.com/gfw_forest_carbon_net_flux/v20210209/raw/per_hectare/",lat,"_",lon,"_net_flux_Mg_CO2e_ha_biomass_soil_forest_extent_2001_19.tif")
  # create string for temporary file
  destfile <- paste0(tempdir(),"/carbon_flux_",lat,"_",lon,".tif")
  # download the file
  download.file(url, destfile)
  # read in temporary raster
  temp.ras <- paste0(tempdir(),"/carbon_flux_",lat,"_",lon,".tif")
  # return file
  return(temp.ras)
}

# example
# get_net_carbon_flux("20S", "060W")
