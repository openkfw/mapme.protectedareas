# big_data_processing.R
# Authors: Johannes Schielein, Om Prakash Bhandari
# Purpose: This script contains several chunks dedicated to process each variables for whole kfw wdpa polygons.

# Contents ------------------------------------------------------- Line Number

# Terrestrial Ecoregions of the World (Ecoregion) ---------------- 40
# Terrestrial Ecoregions of the World (Biome) -------------------- 90
# Mangrove Gain and Loss ----------------------------------------- 150
# Copernicus Global Land Cover ----------------------------------- 270
# World Pop Population Count ------------------------------------- 480
# Terrain Ruggedness Index --------------------------------------- 710
# Global Forest Watch (Area, Loss, CO2) -------------------------- 800


# Source Scripts ---------------------------------------------------------------
source("code/area_proj.R")


# Load required libraries ------------------------------------------------------
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(elevatr)
library(raster)
library(rgdal)










# Terrestrial Ecoregions of the World (Ecoregion) ------------------------------

# load terrestrial ecoregions geopackage
teow <- 
  read_sf("../../datalake/mapme.protectedareas/input/teow/Terrestrial-Ecoregions-World.gpkg")

# load and reproject polygons
pa_polygons_all <- 
  read_sf("../../datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

pa_polygons_all<-
  st_transform(pa_polygons_all, st_crs(teow))

# create function
calculate_teow_intersection_eco <- function(my_pa_polygon) {
  
  # validate the teow polygons
  teow <- st_make_valid(teow)
  # intersect teow with pa polygons
  teow_intersect <- st_intersection(teow,
                                    my_pa_polygon)
  # reproject to LAEA projection
  teow_intersect_proj <- st_transform(teow_intersect, area_proj(my_pa_polygon))
  # calculate area of the intersected portion
  teow_intersect_proj$teow_intersect_sqkm <- as.character(st_area(teow_intersect_proj)/1000000)
  # load as dataframe 
  myData <- as_tibble(teow_intersect_proj)
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

calculate_teow_intersection_eco(pa_polygons_all)






# Terrestrial Ecoregions of the World (Biome) ----------------------------------

# load terrestrial ecoregions geopackage
teow <- 
  read_sf("../../datalake/mapme.protectedareas/input/teow/Terrestrial-Ecoregions-World.gpkg")

# load and reproject polygons
pa_polygons_all <- 
  read_sf("../../datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

pa_polygons_all<-
  st_transform(pa_polygons_all, st_crs(teow))

# create function
calculate_teow_intersection_biome <- function(my_pa_polygon) {
  
  # validate the teow polygons
  teow <- st_make_valid(teow)
  # intersect teow with pa polygons
  teow_intersect <- st_intersection(teow,
                                    my_pa_polygon)
  # reproject to LAEA projection
  teow_intersect_proj <- st_transform(teow_intersect, area_proj(my_pa_polygon))
  # calculate area of the intersected portion
  teow_intersect_proj$biome_intersect_sqkm <- as.character(st_area(teow_intersect_proj)/1000000)
  # load as dataframe 
  myData <- as_tibble(teow_intersect_proj)
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

calculate_teow_intersection_biome(pa_polygons_all)









# Mangrove Gain and Loss -------------------------------------------------------

# load polygons
pa_polygons_all <- 
  read_sf("../../datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

# create function
calculate_mangrove_extent <- function(y) {
  
  # load mangrove gpkg
  mangrove_gpkg <- st_read(paste0("../../datalake/mapme.protectedareas/input/global_mangrove_watch/gmw-v2-",y,"-valid.gpkg"))
  # reproject polygons to match CRS with mangrove
  polygon <- st_transform(pa_polygons_all, st_crs(mangrove_gpkg))
  # intersect mangrove with polygon
  mangrove_subset <- st_intersection(mangrove_gpkg,
                                     polygon)
  # reproject to LAEA projection
  mangrove_subset <- st_transform(mangrove_subset, 
                                  st_crs(area_proj(polygon)))
  # calculate area of the intersected portion
  mangrove_subset$area_sqkm <- st_area(mangrove_subset)/1000000
  # load as dataframe 
  mangrove_data <- as_tibble(mangrove_subset)
  # rename the column to store value per year
  names(mangrove_data)[names(mangrove_data) == "area_sqkm"] <- paste0("mangrove_area_sqkm_",y)
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

#calculate_mangrove_extent("2007")

# process all the years
lapply(c("1996", "2007", "2008", "2009", "2010", "2015", "2016"), FUN = calculate_mangrove_extent)


# Carbon Balance ---------------------------------------------------------------

# load raster
carbonflux_raster <- 
  rast("../../datalake/mapme.protectedareas/input/net_carbon_flux/LA.tif")

# load and reproject polygons  
pa_polygons_all<-
  read_sf("../../datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

pa_polygons_all<-
  st_transform(pa_polygons_all, "+proj=longlat +datum=WGS84 +no_defs")

# create function
calculate_carbon_by_polygon <- function(my_pa_polygon) {
  
  tryCatch(
    {
      # vectorize polygon in order to crop
      my_pa_polygon_vectorized <- vect(my_pa_polygon)
      # crop carbon balance raster based on polygon
      my_pa_polygon_vectorized_crop <- terra::crop(carbonflux_raster, 
                                                   my_pa_polygon_vectorized)
      # mask carbon balance raster to its polygon
      my_pa_polygon_vectorized_mask <- terra::mask(my_pa_polygon_vectorized_crop, 
                                                   my_pa_polygon_vectorized)
      # rasterize polygon based on extent of the cropped carbon raster
      pa_polygon_raster <- terra::rasterize(my_pa_polygon_vectorized, 
                                            my_pa_polygon_vectorized_mask, 
                                            my_pa_polygon_vectorized$WDPAID)
      # calculate zonal statistics: here total cabon balance
      zstats <- terra::zonal(my_pa_polygon_vectorized_mask, 
                             pa_polygon_raster, 
                             fun='sum', 
                             na.rm=T)
      # create dataframe from results
      df.zstats <- data.frame(WDPAID=NA,
                              carbon_balance_MgCo2_per_ha=NA)
      # rename columns
      colnames(zstats) <- colnames(df.zstats)
      # pivot to long format
      zstats_long <- pivot_longer(zstats, 
                                  cols=carbon_balance_MgCo2_per_ha)
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

# create a dataframe to receive results processing first polygon
df_carbon_polygon<-
  calculate_carbon_by_polygon(pa_polygons_all[1,])

# process the rest of the polygons and bind the results to be one large sf object. 
for (i in 2:nrow(pa_polygons_all)) {
  
  df_carbon_polygon <- 
    rbind(df_carbon_polygon,
          calculate_carbon_by_polygon(pa_polygons_all[i, ]))
  print(paste("Done processing line", i, sep=" "))
}

# write results to disk
write.csv(df_carbon_polygon, 
          file="../../datalake/mapme.protectedareas/output/polygon/net_carbon_flux/carbon_balance_allPAs.csv",
          row.names = F)





# Copernicus Global Land Cover -------------------------------------------------

# load land cover raster for specific year - for this routine 2015
lc <- rast("../../datalake/mapme.protectedareas/input/copernicus_global_land_cover/2015/Latin_America_LC_2015.tif")
# load sample polygon as spatVector for terra compatibility
p <- vect("../../datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
# create function from here
lc_area_per_polygon <- function(p) {
  
  tryCatch(
    
    {
      # get the polygon passed to function
      p_v <- p
      # crop raster by polygon - gives raster as bounding box layer
      p_crop <- terra::crop(lc, p_v)
      # mask the raster by polygon - gives rasters as masked layer
      p_mask <- terra::mask(p_crop, p_v)
      # store raster values as dataframe
      df <- as.data.frame(p_mask)
      # new dataframe with value column - to change the column name to value
      df.new <- data.frame(value=NA)
      # rename column to match with new df where raster values are stored
      colnames(df) <- colnames(df.new)
      # area of masked raster
      area_sqm <- terra::area(p_mask)
      # area in km
      area_sqkm <- area_sqm/1000000
      # area per row of dataframe
      area_sqkm_per_cell <- area_sqkm/nrow(df)
      # get WDPAID of the polygon
      id <- p$WDPAID
      # create empty df to receive results
      df.final <- data.frame(WDPAID=id)
      
      # discrete classification and respective area computation - map code represents respective class name
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
df.final_long <- lc_area_per_polygon(p[1, ])

# process other polygons
for (i in 2:nrow(p)) {
  
  df.final_long <- 
    rbind(df.final_long,
          lc_area_per_polygon(p[i, ]))
  print(paste("Done processing line", i, sep=" "))
}


# finally write the results to disk
write.csv(df.final_long,
          file="../../datalake/mapme.protectedareas/output/polygon/copernicus_land_cover/copernicus_2015.csv",
          row.names = F)

# Similarly, we can follow this routine to get the output for the year 2016 - 2019
# by simply replacing the raster in terra object `lc` with desired year raster and 
# replacing the column names ending with `2015` by the desired year














# World Pop Population Count ---------------------------------------------------

# load polygon
p <- 
  vect("../../datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

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
  pa_polygon_raster <- terra::rasterize(my_pa_polygon, my_pa_polygon_mask, wdpa_pid)
  # calculate zonal statistics: here total number of people within a polygon
  zstats <- terra::zonal(my_pa_polygon_mask, 
                         pa_polygon_raster, 
                         fun='sum', 
                         na.rm=T)
  if (y == 2000) {
    
    # create dataframe from results
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2000=NA)
    # rename columns
    colnames(zstats) <- colnames(df.zstats)
    # pivot to long format
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2000)
  } else if (y == 2001) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2001=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2001)
  } else if (y == 2002) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2002=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2002)
  } else if (y == 2003) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2003=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2003)
  } else if (y == 2004) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2004=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2004)
  } else if (y == 2005) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2005=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2005)
  } else if (y == 2006) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2006=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2006)
  } else if (y == 2007) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2007=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2007)
  } else if (y == 2008) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2008=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2008)
  } else if (y == 2009) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2009=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2009)
  } else if (y == 2010) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2010=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2010)
  } else if (y == 2011) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2011=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2011)
  } else if (y == 2012) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2012=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2012)
  } else if (y == 2013) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2013=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2013)
  } else if (y == 2014) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2014=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2014)
  } else if (y == 2015) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2015=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2015)
  } else if (y == 2016) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2016=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2016)
  } else if (y == 2017) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2017=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2017)
  } else if (y == 2018) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2018=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2018)
  } else if (y == 2019) {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2019=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2019)
  } else {
    
    df.zstats <- data.frame(WDPAID=NA,
                            population_count_2020=NA)
    colnames(zstats) <- colnames(df.zstats)
    zstats_long <- pivot_longer(zstats, 
                                cols=population_count_2020)
  }
  # delete temporary files
  delfiles <- dir(path=tempdir() ,pattern="spat_*")
  file.remove(file.path(tempdir(), delfiles))
  # return results
  return(zstats_long)
}


# process for all the polygons and all years and bind the results 

for (i in 2000:2020) {
  
  # create a dataframe to receive results processing first polygon
  df_world_pop <- calculate_pop_count(i, p[1, ])
  
  for (j in 2:nrow(p)) {
    
    df_world_pop <- 
      rbind(df_world_pop,
            calculate_pop_count(i, p[j, ]))
    print(paste("Done processing line", j, sep=" "))
  }
  write.csv(df_world_pop,
            file=paste0("../../datalake/mapme.protectedareas/output/polygon/world_pop/world_pop_per_wdpaid_",i,".csv"),
            row.names = F)
  print(paste("Done processing for year:", i, sep=" "))
}

# rbind all output

l <- list.files("../../datalake/mapme.protectedareas/output/polygon/world_pop/", full.names = T)

r <- rbind(read.csv(l[1]), read.csv(l[2]), read.csv(l[3]), read.csv(l[4]), read.csv(l[5]), read.csv(l[6]), read.csv(l[7]), read.csv(l[8]),
           read.csv(l[9]), read.csv(l[10]), read.csv(l[11]), read.csv(l[12]), read.csv(l[13]), read.csv(l[14]), read.csv(l[15]), read.csv(l[16]),
           read.csv(l[17]), read.csv(l[18]), read.csv(l[19]), read.csv(l[20]), read.csv(l[21]))

write.csv(r, 
          file="../../datalake/mapme.protectedareas/output/polygon/world_pop/world_pop_per_wdpaid.csv",
          row.names = F)












# Terrain Ruggedness Index -----------------------------------------------------

# load PA polygons
p <- 
  readOGR("../../datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

# create function to get TRI values
get_tri <- function(my_pa_polygon) {
  
  tryCatch(
    {
      
      # get elevation raster at zoom level 12
      elevation <- get_elev_raster(my_pa_polygon,
                                   z=12)
      # crop the elevation raster
      elevation_cropped <- crop(elevation,
                                my_pa_polygon)
      # mask the elevation raster by polygon
      elevation_masked <- mask(elevation_cropped,
                               my_pa_polygon)
      # compute TRI
      tri <- terrain(elevation_masked,
                     opt="TRI",
                     unit="degrees",
                     neighbors=8)
      # get wdpa_piid from the polygon
      wdpapid <- as.numeric(my_pa_polygon$WDPA_PID)
      # rasterize the polygon
      r <- rasterize(my_pa_polygon,
                     elevation_masked,
                     wdpapid)
      # compute zonal stats for TRI
      ## mean
      tri_mean <- zonal(tri, r, 'mean', na.rm=T)
      ## median
      tri_median <- zonal(tri, r, 'median', na.rm=T)
      ## standard deviation
      tri_sd <- zonal(tri, r, 'sd', na.rm=T)
      # compute mean elevation values
      elevation_mean <- zonal(elevation_masked, r, 'mean', na.rm=T)
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
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# receive result for one polygon
df.tri <- get_tri(p[1, ])

# process for all the polygons and all years and bind the results 

for (i in 2) {
  
  df.tri <- 
    rbind(df.tri,
          get_tri(p[i, ]))
  print(paste("Done processing line", i, sep=" "))
}

# write results to disk
write.csv(df.tri, 
          file="../../datalake/mapme.protectedareas/output/polygon/terrain_ruggedness_index/terrain_ruggedness_index.csv",
          row.names = F)













# Global Forest Watch (Area, Loss, CO2) -----------------------------------------------------------------------------------

# get the file paths to the raster files
treeCover = "../../datalake/mapme.protectedareas/input/global_forest_watch/treecover2000.tif"
lossYear = "../../datalake/mapme.protectedareas/input/global_forest_watch/lossyear.tif"
co2Layer = ".../../datalake/mapme.protectedareas/input/global_forest_watch/co2_emission_.tif"

# read the region of interest
roi = st_read("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
roi <- st_transform(roi, "+proj=longlat +datum=WGS84 +no_defs")

grass = "/usr/lib/grass78"

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
                       outdir = "../../datalake/mapme.protectedareas/processing/global_forest_watch/")

# write zonal statistics to disk as csv
write.csv(roi_stats,
          file = "../../datalake/mapme.protectedareas/output/polygon/global_forest_watch/roi_stats.csv",
          rrow.names = F)

# receive the results as long table format (area, loss, co2)
# load the csv
g <- read_csv("../../datalake/mapme.protectedareas/output/polygon/global_forest_watch/roi_stats.csv")
# subset only needed columns
wdpa_pid <- g[2]
g_area <- g[50:70]
g_loss <- g[71:91]
g_co2 <- g[92:112]

# bind all columns together
g_bind <- cbind(wdpa_pid, g_area, g_loss, g_co2)
# remove the rows with NA values
g_bind_final <- g_bind[complete.cases(g_bind), ]
# pivot to longer format
g_bind_long <- pivot_longer(g_bind_final,
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
