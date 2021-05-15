# Contents ----------------- Line Number

# World Pop ---------------- 10
# Land Cover Copernicus ---- 80
# Global Mangrove Watch ---- 120
# Net Carbon flux ---------- 160
# TEOW --------------------- 200


# World Pop [Population Count] ------------------------------------------------------------------------------------------------------------

library(sf)
library(dplyr)
library(splus2R)

# load polygon
p <- read_sf("../../../datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
# get ISO of unique countries
countries <- unique(p[["ISO3"]])

# Unconstrained individual countries UN adjusted 2000-2020 (100 m)

get_worldPop <- function(ISO, y) {
  
  tryCatch(
    {
      
      # create the url
      url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/",y,"/",ISO,"/",lowerCase(paste0(ISO)),"_ppp_",y,"_UNadj.tif")
      # create string for temporary file - string should be in temp folder
      destfile <- paste0("/home/rstudio/shared/datalake/mapme.protectedareas/input/world_pop/unconstr_UNadj_",ISO,"_",y,".tif")
      # download the file
      download.file(url, destfile)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}
# test the function
#get_worldPop("GUY", 2000)

for (i in 1:16) {
  
  for (j in 2000:2020) {
    get_worldPop(countries[i], j)
    print(paste("Done processing line", j, sep=" "))
  }
  print(paste("Done processing for country:", countries[i], sep=" "))
}

# Global mosaics 2000-2020

get_worldPop_global <- function(y) {
  
  tryCatch(
    {
      # create the url
      url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/",y,"/0_Mosaicked/ppp_",y,"_1km_Aggregated.tif")
      # create string for temporary file - string should be in temp folder
      destfile <- paste0("/home/rstudio/shared/datalake/mapme.protectedareas/input/world_pop/global_mosaic",y,".tif")
      # download the file
      download.file(url, destfile)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

for (i in 2000:2020) {
  get_worldPop_global(i)
}






# Land Cover Copernicus ----------------------------------------------------------------------------------------------------------------

get_copernicus_land_cover <- function(y, m, g) {
  
  # create url
  url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",y,"/",g,"/",g,"_PROBAV_LC100_global_v3.0.1_",y,"-",m,"_Discrete-Classification-map_EPSG-4326.tif")
  # create string for temporary file
  destfile <- paste0("../../datalake/mapme.protectedareas/input/copernicus_global_land_cover/",y,"/",g,".tif")
  # download the file and save it to tempfolder
  download.file(url, destfile)
  print("download completed!")
}

# test
#get_copernicus_land_cover(2019, "nrt", "W100N00")

# NOTE:
# if 'y' is 2015; 'm' should be 'base'
# if 'y' is 2016, 2017 & 2018; 'm' should be 'conso'
# if 'y' is 2019; 'm' should be 'nrt'
# Total grids:

# W120N40 - W100N40 - W080N40
# W120N20 - W100N20 - W080N20 - W060N20 - W040N20
# W100N00 - W080N00 - W060N00 - W040N00
# W100S20 - W080S20 - W060S20
# W080S40 - W060S40













# Global Mangrove Watch ------------------------------------------------------------------------------------------------------------------------

get_global_mangrove_watch <- function(y) {
  
  tryCatch(
    {
      # create url
      url <- paste0("https://wcmc.io/GMW_",y)
      # create string for temporary file
      destfile <- paste0("/home/rstudio/shared/datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-",y,".zip")
      # download the file and save it to temp folder
      download.file(url, destfile)
      # unzip
      unzip(zipfile = paste0("/home/rstudio/shared/datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-",y,".zip"), 
            exdir = "/home/rstudio/shared/datalake/mapme.protectedareas/input/global_mangrove_watch/")
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# test
#get_global_mangrove_watch("1996")

# NOTE: 
# The function saves the raw data from global mangrove watch as shapefile in zip and then unzip to the separate folder.
# data available for years: 2007, 2008, 2009, 2010, 2015 & 2016












# Net Carbon Flux Raster ------------------------------------------------------------------------------------------------------------------

get_net_carbon_flux <- function(lat, lon) {
  
  # create url
  url <- paste0("https://gfw-data-lake.s3.amazonaws.com/gfw_forest_carbon_net_flux/v20210209/raw/per_hectare/",lat,"_",lon,"_net_flux_Mg_CO2e_ha_biomass_soil_forest_extent_2001_19.tif")
  # create string for temporary file
  destfile <- paste0("/home/rstudio/shared/datalake/mapme.protectedareas_net-forest-carbon-flux-",lat,"-",lon,"_raw.tif")
  # download the file and save it to tempfolder
  download.file(url, destfile)
}

# test
#get_net_carbon_flux("30N", "120W")

# Note: Below given is the range of grids to cover the extent of Latin America
# 20N_60W to 20N_100W
# 10N_50W to 10N_100W
# 00N_40W to 00N_100W
# 10S_40W to 10S_80W

# 20S_50W to 20S_80W
# 30S_60W to 30S_80W
# 40S_70W to 40S_80W
# 50S_60W to 50S_80W















# Terrestrial Ecoregions of the World ------------------------------------------------------------------------------------------------------

# create URL
url <- paste0("https://opendata.arcgis.com/datasets/af92f20b1b43479581c819941e0f75ea_0.zip?outSR=%7B%22falseM%22%3A-100000%2C%22xyTolerance%22%3A8.983152841195215e-9%2C%22mUnits%22%3A10000%2C%22zUnits%22%3A10000%2C%22latestWkid%22%3A4326%2C%22zTolerance%22%3A0.001%2C%22wkid%22%3A4326%2C%22xyUnits%22%3A11258999068426.24%2C%22mTolerance%22%3A0.001%2C%22falseX%22%3A-400%2C%22falseY%22%3A-400%2C%22falseZ%22%3A-100000%7D")
# destination file to download the zip file
destfile <- paste0("/home/rstudio/shared/datalake/mapme.protectedareas/input/teow/teow_global.zip")
# download command
download.file(url, destfile)
# unzip
unzip(zipfile = "/home/rstudio/shared/datalake/mapme.protectedareas/input/teow/teow_global.zip",
      exdir = "/home/rstudio/shared/datalake/mapme.protectedareas/input/teow/")

# Note:
# The above script downloads the TEOW data as zip and then unzip to get the shapefiles. Later, you can write to disk as geopackage.



