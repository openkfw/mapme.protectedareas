# copernicus-land-cover.R
# Authors: "Om Prakash Bhandari (Author), Johannes Schielein (Review)"
# Purpose: This script contains function which takes dataframe (which stores raster values) as argument 
# and returns the data frame in long table format with with areas of land cover classes

# load required libraries ------------------------------------------------------
library(dplyr)
library(tidyverse)

# create a function here
lc_classes <- function(df, y) {
  
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
