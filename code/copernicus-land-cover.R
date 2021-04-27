# copernicus-land-cover.R
# Authors: Om Prakash Bhandari, Johannes Schielein
# Purpose: This script contains function to compute area of different land cover classes (i.e. 23 discrete classification) from Copernicus global land cover rasters...
# ... for individual years from 2015 to 1019.

# Global Land Cover <multiple WDPAIDs> ---------------------------------------------------------------------------------------------------------------------------------
library(terra)
library(sf)
library(wdpar)
library(dplyr)
library(tidyverse)

# load land cover raster
lc <- rast("../../datalake/mapme.protectedareas/input/copernicus_global_land_cover/2015/W060N00.tif")
# load sample polygon
bra <- wdpa_fetch("BRA")%>%
  filter(WDPAID %in% c(34021, 2221, 34004))
# reproject to WGS84
bra <- st_transform(bra, "+proj=longlat +datum=WGS84 +no_defs")

# create function `lc_area_per_polygon`
# takes argument - one polygon at a time
lc_area_per_polygon <- function(p) {
  
  tryCatch(
    
    {
      # spatVector for terra compatibility
      p_v <- vect(p)
      # crop raster by polygon
      p_crop <- terra::crop(lc, p_v)
      # mask the raster by polygon 
      p_mask <- terra::mask(p_crop, p_v)
      # raster values as dataframe
      df <- as.data.frame(p_mask)
      # new dataframe with value column
      df.new <- data.frame(value=NA)
      # rename column to match with new df
      colnames(df) <- colnames(df.new)
      # area of masked raster
      area_sqm <- terra::area(p_mask)
      # in kms
      area_sqkm <- area_sqm/1000000
      # area per row of dataframe
      area_sqkm_per_cell <- area_sqkm/nrow(df)
      # get WDPAID of the polygon
      id <- p$WDPAID
      # empty df to receive results
      df.final <- data.frame(WDPAID=id)
      
      # discrete classification and respective area computation
      ### empty classes
      empty <- df%>%
        filter(value %in% 0)%>%
        nrow()
      df.final$copernicus_lc_empty_area_sqkm_2015 <- area_sqkm_per_cell*empty
      ### 111 Closed forest, evergreen needle leaf
      cfenl <- df%>%
        filter(value %in% 111)%>%
        nrow()
      df.final$copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm_2015 <- area_sqkm_per_cell*cfenl
      ### 113 closed forest, deciduous needle leaf
      cfdnl <- df%>%
        filter(value %in% 113)%>%
        nrow()
      df.final$copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm_2015 <- area_sqkm_per_cell*cfdnl
      ### 112 closed forest, evergreen, broad leaf
      cfebl <- df%>%
        filter(value %in% 112)%>%
        nrow()
      df.final$copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm_2015 <- area_sqkm_per_cell*cfebl
      ### 114 closed forest, deciduous broad leaf
      cfdbl <- df%>%
        filter(value %in% 114)%>%
        nrow()
      df.final$copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm_2015 <- area_sqkm_per_cell*cfdbl
      ### 115 closed forest, mixed
      cfm <- df%>%
        filter(value %in% 115)%>%
        nrow()
      df.final$copernicus_lc_closed_forest_mixed_area_sqkm_2015 <- area_sqkm_per_cell*cfm
      ### 116 closed forest, unknown
      cfu <- df%>%
        filter(value %in% 116)%>%
        nrow()
      df.final$copernicus_lc_closed_forest_unknown_area_sqkm_2015 <- area_sqkm_per_cell*cfu
      ### 121 Open forest, evergreen needle leaf
      ofenl <- df%>%
        filter(value %in% 121)%>%
        nrow()
      df.final$copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm_2015 <- area_sqkm_per_cell*ofenl
      ### 123 Open forest, deciduous needle leaf
      ofdnl <- df%>%
        filter(value %in% 123)%>%
        nrow()
      df.final$copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm_2015 <- area_sqkm_per_cell*ofdnl
      ### 122 Open forest, evergreen broad leaf
      ofebl <- df%>%
        filter(value %in% 122)%>%
        nrow()
      df.final$copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm_2015 <- area_sqkm_per_cell*ofebl
      ### 124 Open forest, deciduous broad leaf
      ofdbl <- df%>%
        filter(value %in% 124)%>%
        nrow()
      df.final$copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm_2015 <- area_sqkm_per_cell*ofdbl
      ### 125 Open forest, mixed
      ofm <- df%>%
        filter(value %in% 125)%>%
        nrow()
      df.final$copernicus_lc_open_forest_mixed_area_sqkm_2015 <- area_sqkm_per_cell*ofm
      ### 126 Open forest, unknown
      ofu <- df%>%
        filter(value %in% 126)%>%
        nrow()
      df.final$copernicus_lc_open_forest_unknown_area_sqkm_2015 <- area_sqkm_per_cell*ofu
      ### 20 Shrubs
      shrubs <- df%>%
        filter(value %in% 20)%>%
        nrow()
      df.final$copernicus_lc_shrubs_area_sqkm_2015 <- area_sqkm_per_cell*shrubs
      ### 30 herbaceous vegetation
      herb_veg <- df%>%
        filter(value %in% 30)%>%
        nrow()
      df.final$copernicus_lc_herbaceous_vegetation_area_sqkm_2015 <- area_sqkm_per_cell*herb_veg
      ### 90 herbaceous wetland
      herb_wet <- df%>%
        filter(value %in% 90)%>%
        nrow()
      df.final$copernicus_lc_herbaceous_wetland_area_sqkm_2015 <- area_sqkm_per_cell*herb_wet
      ### 100 Moss and lichen
      moss <- df%>%
        filter(value %in% 100)%>%
        nrow()
      df.final$copernicus_lc_moss_area_sqkm_2015 <- area_sqkm_per_cell*moss
      ### 60 Bare / sparse vegetation
      bsv <- df%>%
        filter(value %in% 60)%>%
        nrow()
      df.final$copernicus_lc_bare_sparse_vegetation_area_sqkm_2015 <- area_sqkm_per_cell*bsv
      ### 40 Cultivated and managed vegetation/agriculture (cropland)
      cmv <- df%>%
        filter(value %in% 40)%>%
        nrow()
      df.final$copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm_2015 <- area_sqkm_per_cell*cmv
      ### 50 Urban / Built up
      urban <- df%>%
        filter(value %in% 50)%>%
        nrow()
      df.final$copernicus_lc_urban_built_up_area_sqkm_2015 <- area_sqkm_per_cell*urban
      ### 70 Snow and Ice
      snow <- df%>%
        filter(value %in% 70)%>%
        nrow()
      df.final$copernicus_lc_snow_and_ice_area_sqkm_2015 <- area_sqkm_per_cell*snow
      ### 80 Permanent water bodies
      pwb <- df%>%
        filter(value %in% 80)%>%
        nrow()
      df.final$copernicus_lc_permanent_water_bodies_area_sqkm_2015 <- area_sqkm_per_cell*pwb
      ### 200 Open Sea
      sea <- df%>%
        filter(value %in% 200)%>%
        nrow()
      df.final$copernicus_lc_open_sea_area_sqkm_2015 <- area_sqkm_per_cell*sea
      # pivot resulting dataframe to long format
      df.final_long <- pivot_longer(df.final,
                                    cols=c(copernicus_lc_empty_area_sqkm_2015, copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm_2015,
                                           copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm_2015, copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm_2015,
                                           copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm_2015, copernicus_lc_closed_forest_mixed_area_sqkm_2015,
                                           copernicus_lc_closed_forest_unknown_area_sqkm_2015, copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm_2015,
                                           copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm_2015, copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm_2015,
                                           copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm_2015, copernicus_lc_open_forest_mixed_area_sqkm_2015,
                                           copernicus_lc_open_forest_unknown_area_sqkm_2015, copernicus_lc_shrubs_area_sqkm_2015,
                                           copernicus_lc_herbaceous_vegetation_area_sqkm_2015, copernicus_lc_herbaceous_wetland_area_sqkm_2015,
                                           copernicus_lc_moss_area_sqkm_2015, copernicus_lc_bare_sparse_vegetation_area_sqkm_2015,
                                           copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm_2015, copernicus_lc_urban_built_up_area_sqkm_2015,
                                           copernicus_lc_snow_and_ice_area_sqkm_2015, copernicus_lc_permanent_water_bodies_area_sqkm_2015,
                                           copernicus_lc_open_sea_area_sqkm_2015))
      # return the long table data frame
      return(df.final_long)
    },
  error = function(e) {
    message('Error in this line!')
  }
  )
}
  

# example run
df.final_long <- lc_area_per_polygon(bra[1, ])

# process other polygons
for (i in 2:nrow(bra)) {
  
  df.final_long <- 
    rbind(df.final_long,
          lc_area_per_polygon(bra[i, ]))
  print(paste("Done processing line", i, sep=" "))
}


# finally write the results to disk
write.csv(df.final_long,
          file="../../datalake/mapme.protectedareas/output/polygon/copernicus_land_cover/copernicus_2015.csv",
          row.names = F)








# Function `lc_classes` for single polygon to be used in `.rmd` ------------------------------------------------------------------------------------------------------
# takes dataframe as argument: df which is converted from masked raster

lc_classes <- function(df) {
  
  # empty classes
  empty <- df%>%
    filter(value %in% 0)%>%
    nrow()
  df.final$copernicus_lc_empty_area_sqkm_2015 <- area_sqkm_per_cell*empty
  ### 111 Closed forest, evergreen needle leaf
  cfenl <- df%>%
    filter(value %in% 111)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm_2015 <- area_sqkm_per_cell*cfenl
  ### 113 closed forest, deciduous needle leaf
  cfdnl <- df%>%
    filter(value %in% 113)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm_2015 <- area_sqkm_per_cell*cfdnl
  ### 112 closed forest, evergreen, broad leaf
  cfebl <- df%>%
    filter(value %in% 112)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm_2015 <- area_sqkm_per_cell*cfebl
  ### 114 closed forest, deciduous broad leaf
  cfdbl <- df%>%
    filter(value %in% 114)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm_2015 <- area_sqkm_per_cell*cfdbl
  ### 115 closed forest, mixed
  cfm <- df%>%
    filter(value %in% 115)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_mixed_area_sqkm_2015 <- area_sqkm_per_cell*cfm
  ### 116 closed forest, unknown
  cfu <- df%>%
    filter(value %in% 116)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_unknown_area_sqkm_2015 <- area_sqkm_per_cell*cfu
  ### 121 Open forest, evergreen needle leaf
  ofenl <- df%>%
    filter(value %in% 121)%>%
    nrow()
  df.final$copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm_2015 <- area_sqkm_per_cell*ofenl
  ### 123 Open forest, deciduous needle leaf
  ofdnl <- df%>%
    filter(value %in% 123)%>%
    nrow()
  df.final$copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm_2015 <- area_sqkm_per_cell*ofdnl
  ### 122 Open forest, evergreen broad leaf
  ofebl <- df%>%
    filter(value %in% 122)%>%
    nrow()
  df.final$copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm_2015 <- area_sqkm_per_cell*ofebl
  ### 124 Open forest, deciduous broad leaf
  ofdbl <- df%>%
    filter(value %in% 124)%>%
    nrow()
  df.final$copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm_2015 <- area_sqkm_per_cell*ofdbl
  ### 125 Open forest, mixed
  ofm <- df%>%
    filter(value %in% 125)%>%
    nrow()
  df.final$copernicus_lc_open_forest_mixed_area_sqkm_2015 <- area_sqkm_per_cell*ofm
  ### 126 Open forest, unknown
  ofu <- df%>%
    filter(value %in% 126)%>%
    nrow()
  df.final$copernicus_lc_open_forest_unknown_area_sqkm_2015 <- area_sqkm_per_cell*ofu
  ### 20 Shrubs
  shrubs <- df%>%
    filter(value %in% 20)%>%
    nrow()
  df.final$copernicus_lc_shrubs_area_sqkm_2015 <- area_sqkm_per_cell*shrubs
  ### 30 herbaceous vegetation
  herb_veg <- df%>%
    filter(value %in% 30)%>%
    nrow()
  df.final$copernicus_lc_herbaceous_vegetation_area_sqkm_2015 <- area_sqkm_per_cell*herb_veg
  ### 90 herbaceous wetland
  herb_wet <- df%>%
    filter(value %in% 90)%>%
    nrow()
  df.final$copernicus_lc_herbaceous_wetland_area_sqkm_2015 <- area_sqkm_per_cell*herb_wet
  ### 100 Moss and lichen
  moss <- df%>%
    filter(value %in% 100)%>%
    nrow()
  df.final$copernicus_lc_moss_area_sqkm_2015 <- area_sqkm_per_cell*moss
  ### 60 Bare / sparse vegetation
  bsv <- df%>%
    filter(value %in% 60)%>%
    nrow()
  df.final$copernicus_lc_bare_sparse_vegetation_area_sqkm_2015 <- area_sqkm_per_cell*bsv
  ### 40 Cultivated and managed vegetation/agriculture (cropland)
  cmv <- df%>%
    filter(value %in% 40)%>%
    nrow()
  df.final$copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm_2015 <- area_sqkm_per_cell*cmv
  ### 50 Urban / Built up
  urban <- df%>%
    filter(value %in% 50)%>%
    nrow()
  df.final$copernicus_lc_urban_built_up_area_sqkm_2015 <- area_sqkm_per_cell*urban
  ### 70 Snow and Ice
  snow <- df%>%
    filter(value %in% 70)%>%
    nrow()
  df.final$copernicus_lc_snow_and_ice_area_sqkm_2015 <- area_sqkm_per_cell*snow
  ### 80 Permanent water bodies
  pwb <- df%>%
    filter(value %in% 80)%>%
    nrow()
  df.final$copernicus_lc_permanent_water_bodies_area_sqkm_2015 <- area_sqkm_per_cell*pwb
  ### 200 Open Sea
  sea <- df%>%
    filter(value %in% 200)%>%
    nrow()
  df.final$copernicus_lc_open_sea_area_sqkm_2015 <- area_sqkm_per_cell*sea
  # pivot resulting dataframe to long format
  df.final_long <- pivot_longer(df.final,
                                cols=c(copernicus_lc_empty_area_sqkm_2015, copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm_2015,
                                copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm_2015, copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm_2015,
                                copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm_2015, copernicus_lc_closed_forest_mixed_area_sqkm_2015,
                                copernicus_lc_closed_forest_unknown_area_sqkm_2015, copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm_2015,
                                copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm_2015, copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm_2015,
                                copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm_2015, copernicus_lc_open_forest_mixed_area_sqkm_2015,
                                copernicus_lc_open_forest_unknown_area_sqkm_2015, copernicus_lc_shrubs_area_sqkm_2015,
                                copernicus_lc_herbaceous_vegetation_area_sqkm_2015, copernicus_lc_herbaceous_wetland_area_sqkm_2015,
                                copernicus_lc_moss_area_sqkm_2015, copernicus_lc_bare_sparse_vegetation_area_sqkm_2015,
                                copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm_2015, copernicus_lc_urban_built_up_area_sqkm_2015,
                                copernicus_lc_snow_and_ice_area_sqkm_2015, copernicus_lc_permanent_water_bodies_area_sqkm_2015,
                                copernicus_lc_open_sea_area_sqkm_2015))
  # save results to disk
  write.csv(df.final_long, file = "../../datalake/mapme.protectedareas/processing/copernicus_global_land_cover/test_ven_long.csv", row.names = F)
}

# example run
lc_classes(df.ven)
