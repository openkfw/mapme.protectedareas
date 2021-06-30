# Contents ----------------- Line Number

# World Pop ---------------- 30
# Land Cover Copernicus ---- 100
# Global Mangrove Watch ---- 180
# Net Carbon flux ---------- 270
# Global Forest Watch ------ 320
# TEOW --------------------- 340
# Accessibility ------------ 370

# load all required libraries

library(sf)
library(terra)
library(tidyverse)
library(splus2R)
library(raster)
library(remotes)
remotes::install_github("mapme-initiative/mapme.forest")
library(mapme.forest)









# World Pop [Population Count] ------------------------------------------------------------------------------------------------------------

# load polygon
p <- read_sf("../../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
# get ISO of unique countries
countries <- unique(p[["ISO3"]])

# Unconstrained individual countries UN adjusted 2000-2020 (100 m)
# get_worldPop <- function(ISO, y) {
#   
#   tryCatch(
#     {
#       
#       # create the url
#       url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/",y,"/",ISO,"/",lowerCase(paste0(ISO)),"_ppp_",y,"_UNadj.tif")
#       # create string for temporary file - string should be in temp folder
#       destfile <- paste0("../../datalake/mapme.protectedareas/input/world_pop/unconstr_UNadj_",ISO,"_",y,".tif")
#       # download the file
#       download.file(url, destfile)
#     },
#     error = function(e) {
#       message('Error in this line!')
#     }
#   )
# }
# 
# # download rasters for all countries on the region of interest
# for (i in 1:nrow(countries)) {
#   
#   for (j in 2000:2020) {
#     get_worldPop(countries[i], j)
#     print(paste("Done processing line", j, sep=" "))
#   }
#   print(paste("Done processing for country:", countries[i], sep=" "))
# }


# Global mosaics 2000-2020
get_worldPop_global <- function(y) {
  
  tryCatch(
    {
      # create the url
      url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/",y,"/0_Mosaicked/ppp_",y,"_1km_Aggregated.tif")
      # create string for temporary file - string should be in temp folder
      destfile <- paste0("../../datalake/mapme.protectedareas/input/world_pop/global_mosaic",y,".tif")
      # download the file
      download.file(url, destfile)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# download global mosaics
for (i in 2000:2020) {
  get_worldPop_global(i)
}











# Land Cover Copernicus ----------------------------------------------------------------------------------------------------------------

get_copernicus_land_cover <- function(y, g) {
  
  if (y == 2015) {
    
    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",y,"/",g,"/",g,"_PROBAV_LC100_global_v3.0.1_",y,"-base_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0("../../datalake/mapme.protectedareas/input/copernicus_global_land_cover/",y,"/",g,".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")
    
  } else if (y == 2019) {
    
    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",y,"/",g,"/",g,"_PROBAV_LC100_global_v3.0.1_",y,"-nrt_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0("../../datalake/mapme.protectedareas/input/copernicus_global_land_cover/",y,"/",g,".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")
    
  } else {
    
  # create url
  url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",y,"/",g,"/",g,"_PROBAV_LC100_global_v3.0.1_",y,"-conso_Discrete-Classification-map_EPSG-4326.tif")
  # create string for temporary file
  destfile <- paste0("../../datalake/mapme.protectedareas/input/copernicus_global_land_cover/",y,"/",g,".tif")
  # download the file and save it to tempfolder
  download.file(url, destfile)
  print("download completed!")
  }
}

# create data frame containing grids to download
g <- data.frame(grids = c("W120N40", "W100N40", "W080N40", "W120N20", "W100N20", "W080N20", "W060N20", "W040N20",
                          "W100N00", "W080N00", "W060N00", "W040N00", "W100S20", "W080S20", "W060S20",
                          "W080S40", "W060S40"))

# download rasters for all Latin American countries
for (i in 2015:2019) {
  
  for (j in 1:nrow(g)) {
    
    get_copernicus_land_cover(i, toString(g$grids[j]))
  }
  print(paste("Done processing line for year:", i, sep=" "))
}

# create a function to merge rasters per year
merge_lc <- function(y) {
  
  # list the files with full names
  r <- list.files(path = paste0("../../datalake/mapme.protectedareas/input/copernicus_global_land_cover/",y,"/"), full.names = T)
  # merge all the rasters and write to disk
  m <- merge(rast(r[1]), rast(r[2]), rast(r[3]), rast(r[4]), rast(r[5]), rast(r[6]), rast(r[7]), rast(r[8]), rast(r[9]), 
             rast(r[10]), rast(r[11]), rast(r[12]), rast(r[13]), rast(r[14]), rast(r[15]), rast(r[16]), rast(r[17]),
            filename=paste0("../../datalake/mapme.protectedareas/input/copernicus_global_land_cover/",y,"/Latin_America_LC_",y,".tif")))
}

# merge rasters per year
for (i in 2015:2019) {
  
  merge_lc(toString(i))
}













# Global Mangrove Watch ------------------------------------------------------------------------------------------------------------------------

get_global_mangrove_watch <- function(y) {
  
  if (y  == 2007) {
    
    tryCatch(
      {
        # create url
        url <- paste0("https://wcmc.io/GMW_",y)
        # create string for temporary file
        destfile <- paste0("../../datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-",y,".zip")
        # download the file and save it to temp folder
        download.file(url, destfile)
        # unzip
        unzip(zipfile = paste0("../../datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-",y,".zip"), 
              exdir = ("../../datalake/mapme.protectedareas/input/global_mangrove_watch/"))
        # load shapefile
        m <- st_read("../../datalake/mapme.protectedareas/input/global_mangrove_watch/GMW_2007_v2.0.shp")
        # write as geopackage
        st_write(m, "../../datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-2007.gpkg")
        # delete unnecessary files
        unlink("../../datalake/mapme.protectedareas/input/global_mangrove_watch/GMW_2007_v2.0.*")
        unlink("../../datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-2007.zip")
      },
      error = function(e) {
        message('Error in this line!')
      }
    )
  } else {
    
  tryCatch(
    {
      # create url
      url <- paste0("https://wcmc.io/GMW_",y)
      # create string for temporary file
      destfile <- paste0("../../datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-",y,".zip")
      # download the file and save it to temp folder
      download.file(url, destfile)
      # unzip
      unzip(zipfile = paste0("../../datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-",y,".zip"), 
            exdir = ("../../datalake/input/global-mangrove-watch/"))
      # load shapefile
      m <- st_read(paste0("../../datalake/mapme.protectedareas/input/global_mangrove_watch/GMW_001_GlobalMangroveWatch_",y,"/01_Data/GMW_",y,"_v2.shp"))
      # write as geopackage
      st_write(m, paste0("../../datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-",y,".gpkg"))
      # delete unnecessary files
      unlink(paste0("../../datalake/mapme.protectedareas/input/global_mangrove_watch/GMW_001_GlobalMangroveWatch_",y), recursive = T)
      unlink(paste0("../../datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-",y,".zip"))
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}
}

# download global mangrove geopackage for the following years 
lapply(c("1996","2007","2008","2009","2010","2015","2016"), FUN = get_global_mangrove_watch)

# create a function to validate the geometry of all the geopackages
makeValid <- function(y) {
  
  # load mangrove geopackage
  p <- read_sf(paste0("shared/datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-",y,".gpkg"))
  # validate the geometry
  pv <- st_make_valid(p)
  # write the new geometry to the disk
  st_write(pv, paste0("shared/datalake/mapme.protectedareas/input/global_mangrove_watch/global-mangrove-watch-",y,"-valid.gpkg"))
}

# validate geometries for all years
lapply(c("1996","2007","2008","2009","2010","2015","2016"), FUN = makeValid)

















# Net Forest Carbon Flux Raster ------------------------------------------------------------------------------------------------------------------

get_net_carbon_flux <- function(g) {
  
  tryCatch(
    {
      
      # create url
      url <- paste0("https://data-api.globalforestwatch.org/dataset/gfw_forest_carbon_net_flux/v20210331/download/geotiff?grid=10/40000&tile_id=",g,"&pixel_meaning=Mg_CO2e_ha")
      # create string for temporary file
      destfile <- paste0("../../datalake/mapme.protectedareas/input/net_forest_carbon_flux/",g,".tif")
      # download the file and save it to tempfolder
      download.file(url, destfile)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# create data frame containing grids to download
g <- data.frame(grids = c("00N_040W", "00N_050W", "00N_060W", "00N_070W", "00N_080W", "00N_090W", "00N_100W",
                          "10N_050W", "10N_060W", "10N_070W", "10N_080W", "10N_090W", "10N_100W", "20N_060W", "20N_070W",
                          "20N_080W", "20N_090W", "20N_100W", "10S_040W", "10S_050W", "10S_060W", "10S_070W", "10S_080W",
                          "20S_050W", "20S_060W", "20S_070W", "20S_080W", "30S_060W", "30S_070W", "30S_080W", "40S_070W",
                          "40S_080W", "50S_060W", "50S_070W", "50S_080W"))


# download rasters for all Latin America countries
for (i in 1:nrow(g)) {
  
  get_net_carbon_flux(toString(g$grids[i]))
}

















# Global Forest Watch (Area, Loss, CO2) ----------------------------------------------------------------------------------------------------

# read in polygons of interest
aoi = st_read("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
# download GFW data for the area of interest (latest rasters)
raster_files = downloadfGFW(shape = aoi,
                            basename = "",
                            dataset = "GFC-2020-v1.8",
                            outdir = "../../datalake/mapme.protectedareas/input/global_forest_watch/",
                            keepTmpFiles = F)










# Terrestrial Ecoregions of the World ------------------------------------------------------------------------------------------------------

# create URL
url <- paste0("https://opendata.arcgis.com/datasets/af92f20b1b43479581c819941e0f75ea_0.zip?outSR=%7B%22falseM%22%3A-100000%2C%22xyTolerance%22%3A8.983152841195215e-9%2C%22mUnits%22%3A10000%2C%22zUnits%22%3A10000%2C%22latestWkid%22%3A4326%2C%22zTolerance%22%3A0.001%2C%22wkid%22%3A4326%2C%22xyUnits%22%3A11258999068426.24%2C%22mTolerance%22%3A0.001%2C%22falseX%22%3A-400%2C%22falseY%22%3A-400%2C%22falseZ%22%3A-100000%7D")
# destination file to download the zip file
destfile <- paste0("../../datalake/mapme.protectedareas/input/teow/teow_global.zip")
# download command
download.file(url, destfile)
# unzip
unzip(zipfile = "../../datalake/mapme.protectedareas/input/teow/teow_global.zip",
      exdir = "../../datalake/mapme.protectedareas/input/teow/")
# load shapefile
teow <- read_sf("../../datalake/mapme.protectedareas/input/teow/Terrestrial_Ecoregions_World.shp")
# write to disk as geopackage
st_write(teow, "../../datalake/mapme.protectedareas/input/teow/Terrestrial_Ecoregions_World.gpkg")















# Accessibility to cities ------------------------------------------------------------------------------------------------------------------

# function to access accessibility rasters for different population layers
get_accessibility <- function(range, index) {
  
  tryCatch(
    {
      # create URL
      url <- paste0("https://ndownloader.figshare.com/files/",index)
      # destination file to download the zip file
      destfile <- paste0("../../datalake/mapme.protectedareas/input/accessibility_to_cities/2015/acc_",range,".tif")
      # download command
      download.file(url, destfile)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# call the function with required population range and its particular index number in the download file
get_accessibility("5k_10k", 14189840)
get_accessibility("10k_20k", 14189837)
get_accessibility("20k_50k", 14189831)
get_accessibility("50k_100k", 14189825)
get_accessibility("100k_200k", 14189819)
get_accessibility("200k_500k", 14189816)
get_accessibility("500k_1mio", 14189810)
get_accessibility("1mio_5mio", 14189807)
get_accessibility("50k_50mio", 14189849)
get_accessibility("5k_110mio", 14189852)
get_accessibility("20k_110mio", 14189843)
