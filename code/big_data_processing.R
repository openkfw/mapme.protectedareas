# big_data_processing.R
# Authors: Johannes Schielein, Om Prakash Bhandari
# Purpose: This script contains several chunks dedicated to process each variables for whole kfw wdpa polygons.


# Source Scripts ---------------------------------------------------------------
source("code/area_proj.R")


# Load required libraries ------------------------------------------------------
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)


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
      # rasterize polygon based on extent of the cropped carbon raster
      pa_polygon_raster <- terra::rasterize(my_pa_polygon_vectorized, 
                                            my_pa_polygon_vectorized_crop, 
                                            my_pa_polygon_vectorized$WDPAID)
      # calculate zonal statistics: here total cabon balance
      zstats <- terra::zonal(my_pa_polygon_vectorized_crop, 
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
