# big_data_processing.R
# Authors: "Om Prakash Bhandari (Author), Johannes Schielein (Review)"
# Purpose: This script contains several chunks dedicated to process each enlisted variables for whole kfw wdpa polygons. 

# Contents ------------------------------------------------------- Line Number

# Terrestrial Ecoregions of the World (Ecoregion) ---------------- 40
# Terrestrial Ecoregions of the World (Biome) -------------------- 100
# Mangrove Gain and Loss ----------------------------------------- 170
# Carbon Balance ------------------------------------------------- 240
# Copernicus Global Land Cover ----------------------------------- 420
# World Pop Population Count ------------------------------------- 860
# Terrain Ruggedness Index --------------------------------------- 970
# Global Forest Watch (Area, Loss, CO2) -------------------------- 1070
# Accessibility to Cities ---------------------------------------- 1150
# Clay content in the soil --------------------------------------- 1230
# Number of fire events occured in a year ------------------------ 1320

# Source Scripts ---------------------------------------------------------------
source("code/area_proj.R")


# Load required libraries ------------------------------------------------------
library(sf)
library(terra)
library(tidyverse)
library(elevatr)
library(raster)
library(rgdal)
library(remotes)
remotes::install_github("mapme-initiative/mapme.forest")
library(mapme.forest)
library(vroom)
library(gdalUtils)





# Terrestrial Ecoregions of the World (Ecoregion) ------------------------------

# load terrestrial ecoregions geopackage
teow <- 
  vect("../../datalake/mapme.protectedareas/input/teow/Terrestrial_Ecoregions_World_validated.gpkg")

# load and reproject polygons
pa_polygons_all <- 
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
# transform to wgs84
pa_polygons_all <- st_transform(pa_polygons_all,
                                "+proj=longlat +datum=WGS84 +no_defs")

# create function
calculate_teow_intersection_eco <- function(my_pa_polygon) {
  
  # convert from terra object to sf object 
  teow <- 
    st_as_sf(teow)
  # intersect teow with pa polygons
  teow_intersect <- st_intersection(teow,
                                    my_pa_polygon)
  # reproject to LAEA projection
  teow_intersect_proj <- 
    st_transform(teow_intersect,
                 area_proj(my_pa_polygon))
  # calculate area of the intersected portion
  teow_intersect_proj$teow_intersect_sqkm <- 
    as.character(st_area(teow_intersect_proj)/1000000)
  # load as dataframe 
  myData <- 
    as_tibble(teow_intersect_proj)
  # select desired columns
  myData_f <- myData %>% 
    select(WDPAID, ECO_NAME, teow_intersect_sqkm)
  # pivot to long format
  myData_long <- pivot_longer(myData_f,
                              cols=c(ECO_NAME, teow_intersect_sqkm))
  # rename the attributes to match global output long table format
  myData_final <- myData_long %>%
    mutate(name = str_c(lead(name),value, sep = '_'), value = lead(value)) %>%
    filter(str_detect(name, '^teow_intersect_sqkm'))
  # write output dataframe to disk
  write.csv(myData_final, 
            file="../../datalake/mapme.protectedareas/output/polygon/teow/teow_long_allPAs_merged_eco.csv",
            row.names = F)
}

#calculate_teow_intersection_eco(pa_polygons_all)











# Terrestrial Ecoregions of the World (Biome) ----------------------------------

# load terrestrial ecoregions geopackage
teow <- 
  vect("../../datalake/mapme.protectedareas/input/teow/Terrestrial_Ecoregions_World_validated.gpkg")

# load and reproject polygons
pa_polygons_all <- 
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
# transform to wgs84
pa_polygons_all<-
  st_transform(pa_polygons_all,
               "+proj=longlat +datum=WGS84 +no_defs")

# create function
calculate_teow_intersection_biome <- function(my_pa_polygon) {
  
  # convert from terra object to sf object 
  teow <- 
    st_as_sf(teow)
  # intersect teow with pa polygons
  teow_intersect <- st_intersection(teow,
                                    my_pa_polygon)
  # reproject to LAEA projection
  teow_intersect_proj <- 
    st_transform(teow_intersect,
                 area_proj(my_pa_polygon))
  # calculate area of the intersected portion
  teow_intersect_proj$biome_intersect_sqkm <- 
    as.character(st_area(teow_intersect_proj)/1000000)
  # load as dataframe 
  myData <- 
    as_tibble(teow_intersect_proj)
  # select desired columns
  myData_f <- myData %>% 
    select(WDPAID, BIOME_NAME, biome_intersect_sqkm)
  # pivot to long format
  myData_long <- pivot_longer(myData_f,
                              cols=c(BIOME_NAME, biome_intersect_sqkm))
  # rename the attributes to match global output long table format 
  value_biome <- myData_long %>%
    filter(name=="BIOME_NAME") %>%
    pull(value)
  myData_final <- myData_long %>%
    filter(name=="biome_intersect_sqkm") %>%
    mutate(a = value_biome,
           name = stringr::str_c(name, "_" ,a)) %>%
    select(-a) %>%
    group_by(WDPAID, name) %>%
    summarise(value=sum(as.numeric(value)))
  # write output dataframe to disk
  write.csv(myData_final, 
            file="../../datalake/mapme.protectedareas/output/polygon/teow/teow_long_allPAs_merged_biome.csv",
            row.names = F)
}

#calculate_teow_intersection_biome(pa_polygons_all)













# Mangrove Gain and Loss -------------------------------------------------------

# load polygons
pa_polygons_all <- 
  read_sf("../../datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

# create function
calculate_mangrove_extent <- function(y) {
  
  # load mangrove gpkg
  mangrove_gpkg <- 
    st_read(paste0("../../datalake/mapme.protectedareas/input/global_mangrove_watch/gmw-v2-",y,"-valid.gpkg"))
  # reproject polygons to match CRS with mangrove
  polygon <- st_transform(pa_polygons_all,
                          st_crs(mangrove_gpkg))
  # intersect mangrove with polygon
  mangrove_subset <- st_intersection(mangrove_gpkg,
                                     polygon)
  # reproject to LAEA projection
  mangrove_subset <- st_transform(mangrove_subset, 
                                  st_crs(area_proj(polygon)))
  # calculate area of the intersected portion
  mangrove_subset$area_sqkm <- 
    st_area(mangrove_subset)/1000000
  # load as dataframe 
  mangrove_data <- 
    as_tibble(mangrove_subset)
  # rename the column to store value per year
  names(mangrove_data)[names(mangrove_data) == "area_sqkm"] <- 
    paste0("mangrove_area_sqkm_",y)
  # select only the WDPAID and mangrove_area_sqkm column 
  mangrove_data_sorted <- mangrove_data %>%
    select(WDPAID, paste0("mangrove_area_sqkm_",y))
  # pivot to long format
  mangrove_long <- pivot_longer(mangrove_data_sorted, 
                                cols=paste0("mangrove_area_sqkm_",y))
  # write results to disk
  write.csv(mangrove_long, 
            file=paste0("../../datalake/mapme.protectedareas/output/polygon/global_mangrove_watch/gmw_v2_long_allPAs_",y,".csv"),
            row.names = F)
}


# # process all the years
# lapply(c("1996", "2007", "2008", "2009", "2010", "2015", "2016"), FUN = calculate_mangrove_extent)
# 
# ## binding all years' CSVs into one CSV file
# # note: in order for vroom to work, you must save the working directory where the csv files are located
# 
# # set working directory to the global mangrove watch in output
# setwd("../../datalake/mapme.protectedareas/output/polygon/global_mangrove_watch/")
# # get all the csv files
# files <- 
#   fs::dir_ls(glob = "gmw_v2_long_*.csv")
# # bind all CSVs
# data <- 
#   vroom(files)
# # set working directory back to mapme.protectedareas
# setwd("../../../../../Om/mapme.protectedareas/")

# # finally write the results to disk
# write.csv(data,
#           file="../../datalake/mapme.protectedareas/output/polygon/global_mangrove_watch/gmw_v2_long_allPAs.csv",
#           row.names = F)






# Carbon Balance ---------------------------------------------------------------

# create index for carbon flux raster grids; use of index makes function easier to run and avoids merging of the multiple rasters...
# ...which might take more time for processing
# using function `gdaltindex` from package `gdalUtils`

# path to store index shapefile
i <- 
  paste("../../datalake/mapme.protectedareas/processing/net_carbon_flux/raster_index/raster_index",".shp",sep="")
# path to the rasters
g <- list.files(path = "../../datalake/mapme.protectedareas/input/net_forest_carbon_flux/",
                pattern = glob2rx("*.tif"),
                full.names = T)
# run the function gdaltindex to create index
gdaltindex(index_file = i, 
           gdal_file = g,
           f = "ESRI Shapefile")
# load index shapefile
index <- 
  read_sf("../../datalake/mapme.protectedareas/processing/net_carbon_flux/raster_index/raster_index.shp")
# transform to wgs84
index <- st_transform(index,
                      "+proj=longlat +datum=WGS84 +no_defs")

# load and reproject polygons  
pa_polygons_all<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
# transform to wgs84
pa_polygons_all<-
  st_transform(pa_polygons_all,
               "+proj=longlat +datum=WGS84 +no_defs")


# create global function
compute_carbon_balance_by_polygon <- function(carbon_rast, p) {
  
  # crop the raster
  carbon_rast_crop <- terra::crop(carbon_rast,
                                  p)
  # vect for terra compatibility
  p_v <- vect(p)
  # mask the raster
  carbon_rast_mask <- terra::mask(carbon_rast_crop,
                                  p_v)
  # rasterize polygon based on extent of the cropped carbon raster
  p_raster <- terra::rasterize(p_v,
                               carbon_rast_mask,
                               p_v$WDPAID)
  # calculate zonal statistics: here total cabon balance
  zstats <- terra::zonal(carbon_rast_mask, 
                         p_raster, 
                         fun='sum', 
                         na.rm=T)
  # create dataframe from results
  df.zstats <- data.frame(WDPA_PID=NA,
                          carbon_balance_MgCo2_per_ha=NA)
  # rename columns
  colnames(zstats) <- 
    colnames(df.zstats)
  # pivot to long format
  zstats_long <- pivot_longer(zstats, 
                              cols=carbon_balance_MgCo2_per_ha)
  # delete temporary files
  delfiles <- dir(path=tempdir() ,pattern="spat_*")
  file.remove(file.path(tempdir(), delfiles))
  # return results
  return(zstats_long)
}

# create local function
compute_carbon_balance <- function(p) {
  
  tryCatch(
    {
      
      # crop the index shapefile with PA polygon
      p_crop <- st_crop(index,
                        p)
      # get path to the raster
      get_path <- 
        p_crop$location
      # number of rasters
      n <- 
        nrow(p_crop)
      # check whether the polygon intersects with single or multiple raster grids
      if (n == 1) {
        
        # load indexed raster
        carbon_rast <- 
          rast(get_path)
        # call global function
        zstats_long <- 
          compute_carbon_balance_by_polygon(carbon_rast,
                                            p)
        return(zstats_long)
        
      } else if (n == 2) {
        
        # load first raster
        carbon_rast1 <- rast(get_path[1])
        # compute zonal stats for first raster
        zstats_long1 <- 
          compute_carbon_balance_by_polygon(carbon_rast1,
                                            p)
        # load second raster
        carbon_rast2 <- 
          rast(get_path[2])
        # compute zonal stats for second raster
        zstats_long2 <- 
          compute_carbon_balance_by_polygon(carbon_rast2,
                                            p)
        # bind both zonal statistics
        zstats_long <- 
          rbind(zstats_long1,
                zstats_long2)
        # return the results
        return(zstats_long)
        
      } else if (n > 2) {
        
        # load first raster
        carbon_rast <- 
          rast(get_path[1])
        # compute zonal stats for first raster
        zstats_long <- 
          compute_carbon_balance_by_polygon(carbon_rast,
                                            p)
        for (i in 2:n) {
          
          carbon_rast <- 
            rast(get_path[i])
          zstats_long <- rbind(zstats_long,
                               compute_carbon_balance_by_polygon(carbon_rast,
                                                                 p))
        }
        # return the results
        return(zstats_long)
        
      } else {
        
        print("Invalid bounding box!")
      }
    },
    
    error = function(e) {
      message('Error in this line!')
    }
  )
}


# # run the function for first polygon 
# df_carbon <- compute_carbon_balance(pa_polygons_all[1,])
# 
# # run the function for other polygons
# for (i in 2:nrow(pa_polygons_all)) {
#   
#   df_carbon <- 
#     rbind(df_carbon,
#           compute_carbon_balance(pa_polygons_all[i, ]))
#   print(paste("Done processing line", i, sep=" "))
# }
# 
# # write results to disk
# write.csv(df_carbon, 
#           file="../../datalake/mapme.protectedareas/output/polygon/net_carbon_flux/carbon_balance_allPAs.csv",
#           row.names = F)













# Copernicus Global Land Cover -------------------------------------------------

# create index for land cover grids; use of index makes function easier to run and avoids merging of the multiple rasters...
# ...which might take more time for processing
# using function `gdaltindex` from package `gdalUtils`

index_per_year <- function(y) {
  
  # path to store index shapefile
  i <- 
    paste("../../datalake/mapme.protectedareas/processing/copernicus_land_cover/raster_index/raster_index_",y,".shp",sep="")
  # path to the rasters
  g <- list.files(path = paste0("../../datalake/mapme.protectedareas/input/copernicus_global_land_cover/",y,"/"),
                  pattern = glob2rx("*.tif"),
                  full.names = T)
  # run the function gdaltindex to create index
  gdaltindex(index_file = i, 
             gdal_file = g,
             f = "ESRI Shapefile")
}

# run function to create index shapefile for rasters 2015 to 2019
lapply(c("2015", "2016", "2017", "2018", "2019"), FUN = index_per_year)

# load and reproject PA polygons
pa_polygons_all <- 
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
# transform to wgs84
pa_polygons_all <- st_transform(pa_polygons_all,
                                "+proj=longlat +datum=WGS84 +no_defs")

# create global function from here
compute_land_cover_per_polygon <- function(lc_rast, p, y) {
  
  # crop the raster
  lc_rast_crop <- terra::crop(lc_rast,
                              p)
  # vect for terra compatibility
  p_v <- 
    vect(p)
  # mask the raster
  lc_rast_mask <- terra::mask(lc_rast_crop,
                              p_v)
  # store raster values as dataframe
  df <- 
    as.data.frame(lc_rast_mask)
  # new dataframe with value column - to change the column name to value
  df.new <- 
    data.frame(value=NA)
  # rename column to match with new df where raster values are stored
  colnames(df) <- 
    colnames(df.new)
  # area of masked raster in km
  area_sqkm <- terra::expanse(lc_rast_mask,
                              unit="km")
  # area per row of dataframe
  area_sqkm_per_cell <- 
    area_sqkm/nrow(df)
  # get WDPAID of the polygon
  id <- 
    p_v$WDPA_PID
  # create empty df to receive results
  df.final <- 
    data.frame(WDPA_PID=id)
  
  # discrete classification and respective area computation - map code represents respective class name
  ### empty classes
  empty <- df%>%
    filter(value %in% 0)%>%
    nrow()
  df.final$copernicus_lc_empty_area_sqkm <- area_sqkm_per_cell*empty
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_empty_area_sqkm"] <- 
    paste0("copernicus_lc_empty_area_sqkm_",y)
  
  ### 111 Closed forest, evergreen needle leaf
  cfenl <- df%>%
    filter(value %in% 111)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm <- area_sqkm_per_cell*cfenl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm"] <- 
    paste0("copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm_",y)
  
  ### 113 closed forest, deciduous needle leaf
  cfdnl <- df%>%
    filter(value %in% 113)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm <- area_sqkm_per_cell*cfdnl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm"] <- 
    paste0("copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm_",y)
  
  ### 112 closed forest, evergreen, broad leaf
  cfebl <- df%>%
    filter(value %in% 112)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm <- area_sqkm_per_cell*cfebl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm"] <- 
    paste0("copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm_",y)
  
  ### 114 closed forest, deciduous broad leaf
  cfdbl <- df%>%
    filter(value %in% 114)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm <- area_sqkm_per_cell*cfdbl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm"] <- 
    paste0("copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm_",y)
  
  ### 115 closed forest, mixed
  cfm <- df%>%
    filter(value %in% 115)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_mixed_area_sqkm <- area_sqkm_per_cell*cfm
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_mixed_area_sqkm"] <- 
    paste0("copernicus_lc_closed_forest_mixed_area_sqkm_",y)
  
  ### 116 closed forest, unknown
  cfu <- df%>%
    filter(value %in% 116)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_unknown_area_sqkm <- area_sqkm_per_cell*cfu
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_unknown_area_sqkm"] <- 
    paste0("copernicus_lc_closed_forest_unknown_area_sqkm_",y)
  
  ### 121 Open forest, evergreen needle leaf
  ofenl <- df%>%
    filter(value %in% 121)%>%
    nrow()
  df.final$copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm <- area_sqkm_per_cell*ofenl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm"] <- 
    paste0("copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm_",y)
  
  ### 123 Open forest, deciduous needle leaf
  ofdnl <- df%>%
    filter(value %in% 123)%>%
    nrow()
  df.final$copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm <- area_sqkm_per_cell*ofdnl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm"] <- 
    paste0("copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm_",y)
  
  ### 122 Open forest, evergreen broad leaf
  ofebl <- df%>%
    filter(value %in% 122)%>%
    nrow()
  df.final$copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm <- area_sqkm_per_cell*ofebl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm"] <- 
    paste0("copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm_",y)
  
  ### 124 Open forest, deciduous broad leaf
  ofdbl <- df%>%
    filter(value %in% 124)%>%
    nrow()
  df.final$copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm <- area_sqkm_per_cell*ofdbl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm"] <- 
    paste0("copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm_",y)
  
  ### 125 Open forest, mixed
  ofm <- df%>%
    filter(value %in% 125)%>%
    nrow()
  df.final$copernicus_lc_open_forest_mixed_area_sqkm <- area_sqkm_per_cell*ofm
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_mixed_area_sqkm"] <- 
    paste0("copernicus_lc_open_forest_mixed_area_sqkm_",y)
  
  ### 126 Open forest, unknown
  ofu <- df%>%
    filter(value %in% 126)%>%
    nrow()
  df.final$copernicus_lc_open_forest_unknown_area_sqkm <- area_sqkm_per_cell*ofu
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_unknown_area_sqkm"] <- 
    paste0("copernicus_lc_open_forest_unknown_area_sqkm_",y)
  
  ### 20 Shrubs
  shrubs <- df%>%
    filter(value %in% 20)%>%
    nrow()
  df.final$copernicus_lc_shrubs_area_sqkm <- area_sqkm_per_cell*shrubs
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_shrubs_area_sqkm"] <- 
    paste0("copernicus_lc_shrubs_area_sqkm_",y)
  
  ### 30 herbaceous vegetation
  herb_veg <- df%>%
    filter(value %in% 30)%>%
    nrow()
  df.final$copernicus_lc_herbaceous_vegetation_area_sqkm <- area_sqkm_per_cell*herb_veg
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_herbaceous_vegetation_area_sqkm"] <- 
    paste0("copernicus_lc_herbaceous_vegetation_area_sqkm_",y)
  
  ### 90 herbaceous wetland
  herb_wet <- df%>%
    filter(value %in% 90)%>%
    nrow()
  df.final$copernicus_lc_herbaceous_wetland_area_sqkm <- area_sqkm_per_cell*herb_wet
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_herbaceous_wetland_area_sqkm"] <- 
    paste0("copernicus_lc_herbaceous_wetland_area_sqkm_",y)
  
  ### 100 Moss and lichen
  moss <- df%>%
    filter(value %in% 100)%>%
    nrow()
  df.final$copernicus_lc_moss_area_sqkm <- area_sqkm_per_cell*moss
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_moss_area_sqkm"] <- 
    paste0("copernicus_lc_moss_area_sqkm_",y)
  
  ### 60 Bare / sparse vegetation
  bsv <- df%>%
    filter(value %in% 60)%>%
    nrow()
  df.final$copernicus_lc_bare_sparse_vegetation_area_sqkm <- area_sqkm_per_cell*bsv
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_bare_sparse_vegetation_area_sqkm"] <- 
    paste0("copernicus_lc_bare_sparse_vegetation_area_sqkm_",y)
  
  ### 40 Cultivated and managed vegetation/agriculture (cropland)
  cmv <- df%>%
    filter(value %in% 40)%>%
    nrow()
  df.final$copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm <- area_sqkm_per_cell*cmv
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm"] <- 
    paste0("copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm_",y)
  
  ### 50 Urban / Built up
  urban <- df%>%
    filter(value %in% 50)%>%
    nrow()
  df.final$copernicus_lc_urban_built_up_area_sqkm <- area_sqkm_per_cell*urban
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_urban_built_up_area_sqkm"] <- 
    paste0("copernicus_lc_urban_built_up_area_sqkm_",y)
  
  ### 70 Snow and Ice
  snow <- df%>%
    filter(value %in% 70)%>%
    nrow()
  df.final$copernicus_lc_snow_and_ice_area_sqkm <- area_sqkm_per_cell*snow
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_snow_and_ice_area_sqkm"] <- 
    paste0("copernicus_lc_snow_and_ice_area_sqkm_",y)
  
  ### 80 Permanent water bodies
  pwb <- df%>%
    filter(value %in% 80)%>%
    nrow()
  df.final$copernicus_lc_permanent_water_bodies_area_sqkm <- area_sqkm_per_cell*pwb
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_permanent_water_bodies_area_sqkm"] <- 
    paste0("copernicus_lc_permanent_water_bodies_area_sqkm_",y)
  
  ### 200 Open Sea
  sea <- df%>%
    filter(value %in% 200)%>%
    nrow()
  df.final$copernicus_lc_open_sea_area_sqkm <- area_sqkm_per_cell*sea
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_sea_area_sqkm"] <- 
    paste0("copernicus_lc_open_sea_area_sqkm_",y)
  
  # pivot resulting dataframe to long format
  df.final_long <- pivot_longer(df.final,
                                cols=c(paste0("copernicus_lc_empty_area_sqkm_",y), paste0("copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm_",y),
                                       paste0("copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm_",y), paste0("copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm_",y),
                                       paste0("copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm_",y), paste0("copernicus_lc_closed_forest_mixed_area_sqkm_",y),
                                       paste0("copernicus_lc_closed_forest_unknown_area_sqkm_",y), paste0("copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm_",y),
                                       paste0("copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm_",y), paste0("copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm_",y),
                                       paste0("copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm_",y), paste0("copernicus_lc_open_forest_mixed_area_sqkm_",y),
                                       paste0("copernicus_lc_open_forest_unknown_area_sqkm_",y), paste0("copernicus_lc_shrubs_area_sqkm_",y),
                                       paste0("copernicus_lc_herbaceous_vegetation_area_sqkm_",y), paste0("copernicus_lc_herbaceous_wetland_area_sqkm_",y),
                                       paste0("copernicus_lc_moss_area_sqkm_",y), paste0("copernicus_lc_bare_sparse_vegetation_area_sqkm_",y),
                                       paste0("copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm_",y), paste0("copernicus_lc_urban_built_up_area_sqkm_",y),
                                       paste0("copernicus_lc_snow_and_ice_area_sqkm_",y), paste0("copernicus_lc_permanent_water_bodies_area_sqkm_",y),
                                       paste0("copernicus_lc_open_sea_area_sqkm_",y)))
  # return results
  return(df.final_long)
}










# create local function from here
compute_land_cover <- function(p, y) {
  
  tryCatch(
    {
      
      # load index shapefile
      index <-
        read_sf(paste0("../../datalake/mapme.protectedareas/processing/copernicus_land_cover/raster_index/raster_index_",y,".shp"))
      # transform to wgs84
      index <- st_transform(index,
                            "+proj=longlat +datum=WGS84 +no_defs")
      # crop the index shapefile with PA polygon
      p_crop <- st_crop(index,
                        p)
      # get path to the raster
      get_path <- 
        p_crop$location
      # number of rasters
      n <- 
        nrow(p_crop)
      # check whether the polygon intersects with single or multiple raster grids
      if (n == 1) {
        
        # load indexed raster
        lc_rast <- 
          rast(get_path)
        # call global function
        zstats_long <- compute_land_cover_per_polygon(lc_rast,
                                                      p,
                                                      y)
        return(zstats_long)
        
      } else if (n == 2) {
        
        # load first raster
        lc_rast1 <- rast(get_path[1])
        # compute zonal stats for first raster
        zstats_long1 <- compute_land_cover_per_polygon(lc_rast1,
                                                       p,
                                                       y)
        # load second raster
        lc_rast2 <- rast(get_path[2])
        # compute zonal stats for second raster
        zstats_long2 <- compute_land_cover_per_polygon(lc_rast2,
                                                       p,
                                                       y)
        # bind both zonal statistics
        zstats_long <- 
          rbind(zstats_long1,
                zstats_long2)
        # return the results
        return(zstats_long)
        
      } else if (n > 2) {
        
        # load first raster
        lc_rast <- rast(get_path[1])
        # compute zonal stats for first raster
        zstats_long <- compute_land_cover_per_polygon(lc_rast,
                                                      p,
                                                      y)
        for (i in 2:n) {
          
          lc_rast <- rast(get_path[i])
          zstats_long <- rbind(zstats_long,
                               compute_land_cover_per_polygon(lc_rast,
                                                              p,
                                                              y))
        }
        # return the results
        return(zstats_long)
        
      } else {
        
        print("Invalid bounding box!")
      }
    },
    
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# # run the function to get the results from 2015 to 2019
# for (i in 2015:2019) {
#   
#   # create a dataframe to receive results processing first polygon
#   df_lc <- compute_land_cover(pa_polygons_all[1,],
#                               i)
#   
#   for (j in 2:nrow(pa_polygons_all)) {
#     
#     df_lc <- 
#       rbind(df_lc,
#             compute_land_cover(pa_polygons_all[j, ],
#                                i))
#     print(paste("Done processing line", j, sep=" "))
#   }
#   
#   write.csv(df_lc,
#             file=paste0("../../datalake/mapme.protectedareas/output/polygon/copernicus_land_cover/copernicus_global_land_cover_allPAs_",i,"_new.csv"),
#             row.names = F)
#   print(paste("Done processing for year:", i, sep=" "))
# }
# 
# ## other way round for binding rows
# # note: in order for vroom to work, you must save the working directory where the csv files are located
# 
# # set working directory to the copernicus land cover
# setwd("../../datalake/mapme.protectedareas/output/polygon/copernicus_land_cover/")
# # get all the csv files
# files <- 
#   fs::dir_ls(glob = "copernicus_*.csv")
# # bind all CSVs
# data <- 
#   vroom(files)
# # set working directory back to mapme.protectedareas
# setwd("../../../../../Om/mapme.protectedareas/")
# 
# # finally write the results to disk
# write.csv(data,
#           file="../../datalake/mapme.protectedareas/output/polygon/copernicus_land_cover/global_land_cover_allPAs.csv",
#           row.names = F)















# World Pop Population Count ---------------------------------------------------

# load polygon
pa_polygons_all <- 
  vect("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

# create function
calculate_pop_count <- function(y, my_pa_polygon) {
  
  
  # load worldpop global mosaic raster
  pop <- 
    rast(paste0("../../datalake/mapme.protectedareas/input/world_pop/global_mosaic",y,".tif"))
  # crop population count raster based on polygon
  my_pa_polygon_crop <- terra::crop(pop,
                                    my_pa_polygon)
  # mask the population count raster
  my_pa_polygon_mask <- terra::mask(my_pa_polygon_crop,
                                    my_pa_polygon)
  # take numeric wdpa_pid
  wdpa_pid <- my_pa_polygon$WDPA_PID%>%
    as.numeric()
  # rasterize polygon based on extent of the cropped population count raster
  pa_polygon_raster <- 
    terra::rasterize(my_pa_polygon,
                     my_pa_polygon_mask,
                     wdpa_pid)
  # calculate zonal statistics: here total number of people within a polygon
  zstats <- terra::zonal(my_pa_polygon_mask, 
                         pa_polygon_raster, 
                         fun='sum', 
                         na.rm=T)
  # create dataframe from results
  df.zstats <- data.frame(WDPAID=NA,
                          population_count=NA)
  # rename columns
  colnames(zstats) <- 
    colnames(df.zstats)
  # rename the column to store value per year
  names(zstats)[names(zstats) == "population_count"] <- 
    paste0("population_count_",y)
  # pivot to long format
  zstats_long <- pivot_longer(zstats, 
                              cols=paste0("population_count_",y))
  # delete temporary files
  delfiles <- dir(path=tempdir() ,pattern="spat_*")
  file.remove(file.path(tempdir(), delfiles))
  # return results
  return(zstats_long)
}


# # process for all the polygons and all years and bind the results 
# 
# for (i in 2000:2020) {
#   
#   # create a dataframe to receive results processing first polygon
#   df_world_pop <- calculate_pop_count(i,
#                                       pa_polygons_all[1, ])
#   
#   for (j in 2:nrow(pa_polygons_all)) {
#     
#     df_world_pop <- 
#       rbind(df_world_pop,
#             calculate_pop_count(i,
#                                 pa_polygons_all[j, ]))
#     print(paste("Done processing line", j, sep=" "))
#   }
#   # write results per year to CSV file in datalake
#   write.csv(df_world_pop,
#             file=paste0("../../datalake/mapme.protectedareas/output/polygon/world_pop/world_pop_per_wdpaid_",i,".csv"),
#             row.names = F)
#   print(paste("Done processing for year:", i, sep=" "))
# }
# 
# ## bind rows all output CSVs with VROOM
# # note: in order for vroom to work, you must save the working directory where the csv files are located
# library(vroom)
# 
# # set working directory to the world pop
# setwd("../../datalake/mapme.protectedareas/output/polygon/world_pop/")
# # get all the csv files
# files <- 
#   fs::dir_ls(glob = "world_pop*.csv")
# # bind all CSVs
# data <- 
#   vroom(files)
# # set working directory back to mapme.protectedareas
# setwd("../../../../../Om/mapme.protectedareas/")
# 
# # write results to disk
# write.csv(data, 
#           file="../../datalake/mapme.protectedareas/output/polygon/world_pop/world_pop_per_wdpaid.csv",
#           row.names = F)
















# Terrain Ruggedness Index -----------------------------------------------------

# load PA polygons
pa_polygons_all <- 
  readOGR("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

# create function to get TRI values
get_terrain_ruggedness_index <- function(my_pa_polygon) {
  
  tryCatch(
    {
      
      # get elevation raster at zoom level 12
      elevation <- get_elev_raster(my_pa_polygon,
                                   z=12)
      # crop the elevation raster
      elevation_cropped <- raster::crop(elevation,
                                        my_pa_polygon)
      # mask the elevation raster by polygon
      elevation_masked <- raster::mask(elevation_cropped,
                                       my_pa_polygon)
      # compute TRI
      tri <- raster::terrain(elevation_masked,
                             opt="TRI",
                             unit="degrees",
                             neighbors=8)
      # get wdpa_pid from the polygon
      wdpapid <- 
        as.numeric(my_pa_polygon$WDPA_PID)
      # rasterize the polygon
      r <- raster::rasterize(my_pa_polygon,
                             elevation_masked,
                             wdpapid)
      # compute zonal stats for TRI
      ## mean
      tri_mean <- 
        raster::zonal(tri, r, 'mean', na.rm=T)
      ## median
      tri_median <- 
        raster::zonal(tri, r, 'median', na.rm=T)
      ## standard deviation
      tri_sd <- 
        raster::zonal(tri, r, 'sd', na.rm=T)
      # compute mean elevation values
      elevation_mean <- 
        raster::zonal(elevation_masked, r, 'mean', na.rm=T)
      # create data frame to receive results
      df.tri <- data.frame(WDPA_PID=wdpapid,
                           terrain_ruggedness_index_mean=tri_mean[ ,2],
                           terrain_ruggedness_index_median=tri_median[ ,2],
                           terrain_ruggedness_index_standard_deviation=tri_sd[ ,2],
                           elevation_mean=elevation_mean[ ,2])
      # pivot to long table format
      df.tri_long <- pivot_longer(df.tri,
                                  cols=c(terrain_ruggedness_index_mean,
                                         terrain_ruggedness_index_median,
                                         terrain_ruggedness_index_standard_deviation,
                                         elevation_mean))
      # delete rasters from temporary directory
      unlink(paste0(tempdir(),"/*tif"), recursive = T)
      # return the results
      return(df.tri_long)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# # receive result for one polygon
# df.tri <- 
#   get_terrain_ruggedness_index(pa_polygons_all[1, ])
# 
# # process for all the polygons and bind the results 
# for (i in 2:nrow(pa_polygons_all)) {
#   
#   df.tri <- 
#     rbind(df.tri,
#           get_terrain_ruggedness_index(pa_polygons_all[i, ]))
#   print(paste("Done processing line", i, sep=" "))
# }
# 
# # write results to disk
# write.csv(df.tri, 
#           file="../../datalake/mapme.protectedareas/output/polygon/terrain_ruggedness_index/terrain_ruggedness_index_allPAs.csv",
#           row.names = F)














# Global Forest Watch (Area, Loss, CO2) -----------------------------------------------------------------------------------
# requires package `mapme.forest`

# get the file paths to the raster files
treeCover = "../../datalake/mapme.protectedareas/input/global_forest_watch/treecover2000.tif"
lossYear = "../../datalake/mapme.protectedareas/input/global_forest_watch/lossyear.tif"
co2Layer = "../../datalake/mapme.protectedareas/input/global_forest_watch/co2_emission_.tif"

# read the region of interest
roi <- 
  st_read("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
# transform roi to wgs84
roi <- 
  st_transform(roi, 
               "+proj=longlat +datum=WGS84 +no_defs")
# path to the installed grass
grass = "/usr/lib/grass78"

# zonal statistics
roi_stats = statsGRASS(grass = grass, 
                       addon_base = "./data-raw/addons", 
                       areas = roi, 
                       tree_cover = treeCover, 
                       tree_loss = lossYear, 
                       tree_co2 = co2Layer, 
                       idcol =  "WDPA_PID", 
                       thresholdClump = 6, 
                       thresholdCover = 10, 
                       years = 2001:2020, 
                       saveRaster = T,
                       outdir = "../../datalake/mapme.protectedareas/processing/")

# write zonal statistics to disk as csv
write.csv(roi_stats,
          file = "../../datalake/mapme.protectedareas/output/polygon/global_forest_watch/roi_stats_allPAs.csv",
          row.names = F)

# receive the results as long table format (area, loss, co2)
# load the csv
g <- 
  read_csv("../../datalake/mapme.protectedareas/output/polygon/global_forest_watch/roi_stats_allPAs.csv")
# subset only needed columns
wdpa_pid <- g[2]
g_area <- g[50:70]
g_loss <- g[71:91]
g_co2 <- g[92:112]

# bind all columns together
g_bind <- 
  cbind(wdpa_pid, g_area, g_loss, g_co2)
# remove the rows with NA values
g_bind_final <- 
  g_bind[complete.cases(g_bind), ]
# pivot to longer format
g_bind_long <- 
  pivot_longer(g_bind_final,
                            cols = c("area_2000", "area_2001", "area_2002", "area_2003", "area_2004", "area_2005", "area_2006", "area_2007",
                                     "area_2008", "area_2009", "area_2010", "area_2011", "area_2012", "area_2013", "area_2014", "area_2015",
                                     "area_2016", "area_2017", "area_2018", "area_2019", "area_2020", "loss_2000", "loss_2001", "loss_2002",
                                     "loss_2003", "loss_2004", "loss_2005", "loss_2006", "loss_2007", "loss_2008", "loss_2009", "loss_2010",
                                     "loss_2011", "loss_2012", "loss_2013", "loss_2014", "loss_2015", "loss_2016", "loss_2017", "loss_2018",
                                     "loss_2019", "loss_2020", "co2_2000", "co2_2001", "co2_2002", "co2_2003", "co2_2004", "co2_2005",
                                     "co2_2006", "co2_2007", "co2_2008", "co2_2009", "co2_2010", "co2_2011", "co2_2012", "co2_2013",
                                     "co2_2014","co2_2015", "co2_2016", "co2_2017", "co2_2018", "co2_2019", "co2_2020"))
# write long format result to disk
write.csv(g_bind_long,
          file = "../../datalake/mapme.protectedareas/output/polygon/global_forest_watch/zonal_statistics_long.csv",
          row.names = F)












# Acessibility to nearby cities -----------------------------------------------------------------------------------

# load accessibility raster
acc_rast <- 
  rast("../../datalake/mapme.protectedareas/input/accessibility_to_cities/2015/acc_50k_100k.tif")

# load and reproject PA polygons
pa_polygons_all <- 
  st_read("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
# transform to wgs84
pa_polygons_all <- 
  st_transform(pa_polygons_all,
               "+proj=longlat +datum=WGS84 +no_defs")
# spatVector for terra compatibility 
pa_polygons_all <- 
  vect(pa_polygons_all)

# create a function ----
compute_accessibility <- function(my_pa_polygon) {
  
  tryCatch(
    {
      # crop raster
      acc_rast_crop <- 
        terra::crop(acc_rast,
                    my_pa_polygon)
      # mask raster
      acc_rast_mask <- 
        terra::mask(acc_rast_crop,
                    my_pa_polygon)
      # get wdpa pid
      wdpapid <- 
        as.numeric(my_pa_polygon$WDPA_PID)
      # rasterize
      my_pa_polygon_rast <-terra::rasterize(my_pa_polygon,
                                            acc_rast_mask,
                                            wdpapid)
      # zonal stats - median
      zstats <- terra::zonal(acc_rast_mask,
                             my_pa_polygon_rast,
                             fun='min',
                             na.rm=T)
      # create data frame to store results
      df <- data.frame(WDPA_PID=wdpapid,
                       travel_time_to_nearby_cities_min=zstats[, 2])
      # pivot to longer format
      zstats_longer <- pivot_longer(df,
                                    cols = travel_time_to_nearby_cities_min)
      # return results
      return(zstats_longer)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# # receive result for first polygon
# df.final <- 
#   compute_accessibility(pa_polygons_all[1,])
# 
# # receive result for other polygons
# for (i in 2:nrow(pa_polygons_all)) {
#   
#   df.final <- 
#     rbind(df.final,
#           compute_accessibility(pa_polygons_all[i, ]))
# }
# 
# # write results to disk
# write.csv(df.final,
#           file = "../../datalake/mapme.protectedareas/output/polygon/accessibility_to_cities/travel_time_to_nearby_cities_2015.csv",
#           row.names = F)







# Clay content in the soil --------------------------------------------------------------------

# load PA polygon
pa_polygons_all <- 
  vect("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

# create a function 
compute_clay_content <- function(b, my_pa_polygon) {
  
  
  tryCatch(
    
    {
      
      # load worldpop global mosaic raster
      clay <- 
        rast(paste0("../../datalake/mapme.protectedareas/input/clay_content/clay_content_",b,"_cm.tif"))
      # crop population count raster based on polygon
      my_pa_polygon_crop <- terra::crop(clay,
                                        my_pa_polygon)
      # mask the population count raster
      my_pa_polygon_mask <- terra::mask(my_pa_polygon_crop,
                                        my_pa_polygon)
      # take numeric wdpa_pid
      wdpa_pid <- my_pa_polygon$WDPA_PID%>%
        as.numeric()
      # rasterize polygon based on extent of the cropped population count raster
      pa_polygon_raster <- 
        terra::rasterize(my_pa_polygon,
                         my_pa_polygon_mask,
                         wdpa_pid)
      # calculate zonal statistics: here total number of people within a polygon
      zstats <- terra::zonal(my_pa_polygon_mask, 
                             pa_polygon_raster,
                             fun='mean',
                             na.rm=T)
      # create dataframe from results
      df.zstats <- data.frame(WDPAID=NA,
                              clay_content=NA)
      # rename columns
      colnames(zstats) <- 
        colnames(df.zstats)
      # rename the column to store value per year
      names(zstats)[names(zstats) == "clay_content"] <- 
        paste0("clay_content_",b,"_cm")
      # pivot to long format
      zstats_long <- pivot_longer(zstats, 
                                  cols=paste0("clay_content_",b,"_cm"))
      # delete temporary files
      delfiles <- dir(path=tempdir() ,pattern="spat_*")
      file.remove(file.path(tempdir(), delfiles))
      # return results
      return(zstats_long)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}


# # process for all the polygons and all availabe level of vertical depths and bind the results
# 
# for (i in c(0, 10, 30)) {
#   
#   # create a dataframe to receive results processing first polygon
#   df_clay <- compute_clay_content(i,
#                                   pa_polygons_all[1, ])
#   
#   for (j in 2:nrow(pa_polygons_all)) {
#     
#     df_clay <- 
#       rbind(df_clay,
#             compute_clay_content(i,
#                                  pa_polygons_all[j, ]))
#     print(paste("Done processing line", j, sep=" "))
#   }
#   # write results per depth to CSV file in datalake
#   write.csv(df_clay,
#             file=paste0("../../datalake/mapme.protectedareas/output/polygon/clay_content/clay_content_",i,"_cm.csv"),
#             row.names = F)
#   print(paste("Done processing for depth (in cm):", i, sep=" "))
# }







# Number of fire events occured in a year ------------------------------------------------------

library(sf)
library(tidyr)
library(tidyverse)
library(parallel)
library(pbmcapply)

# load wdpa polygon
pa_polygon <- 
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_valid.gpkg")
# transform to WGS84
pa_polygon <- st_transform(pa_polygon,
                           "+proj=longlat +datum=WGS84 +no_defs")

for (j in 2012:2020) {
  
  # load active fire polygon
  fire <- 
    read_sf(paste0("../../datalake/mapme.protectedareas/input/fire_event/fire_",j,"_subset.gpkg"))
  # transform to WGS84
  fire <- st_transform(fire,
                       "+proj=longlat +datum=WGS84 +no_defs")
  
  # parallelization from here
  z_stats = pbmclapply(1:nrow(pa_polygon), function(i) {
    
    
    # intersection of fire and WDPA polygon
    sf_p <- 
      st_intersection(fire,
                      pa_polygon[i, ])
    # get wdpaid
    wdpa_pid <- 
      pa_polygon[i, ]$WDPA_PID
    # get number of rows 
    n <- 
      nrow(sf_p)
    # creata a data frame to store result
    df <- 
      data.frame(WDPA_PID=wdpa_pid,
                 fire_events_count=n)
    # rename column name to store value per year
    names(df)[names(df) == "fire_events_count"] <- 
      paste0("fire_events_count_",j)
    # pivot to longer format
    df_long <- 
      tidyr::pivot_longer(df,
                          cols=paste0("fire_events_count_",j))
    # return results
    return(df_long)
  }, mc.cores = 6)
  
  # unlist all
  for (i in 1:length(z_stats)) {
    
    if (class(z_stats[[i]]) != "try-error") {
      
      df_final <-
        as.data.frame(z_stats[[i]])
      write.csv(df_final,
                file = paste0("../../datalake/mapme.protectedareas/output/polygon/fire_event/fire_events_count_",j,"_",i,".csv"),
                row.names = F)
    } else {
      print(paste("try-error in line:", i, sep=" "))
    }
  }
}

