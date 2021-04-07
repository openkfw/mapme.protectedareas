# big_data_processing.R
# Authors: Johannes Schielein, Om Prakash Bhandari
# Purpose: This script contains several chunks dedicated to process each variables for whole kfw wdpa polygon.


# Source Scripts ---------------------------------------------------------------
source("code/area_proj.R")


# Load required libraries ------------------------------------------------------
library(sf)
library(terra)
library(dplyr)
library(tidyr)


# Terrestrial Ecoregions of the World (all wdpa)--------------------------------

t <- st_read("/home/ombhandari/shared/datalake/mapme.protectedareas/input/teow/Terrestrial-Ecoregions-World.gpkg")
t <- st_make_valid(t)
p <- st_read("/home/ombhandari/shared/datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_allPAs.gpkg")
p <- st_transform(p, st_crs(t))
tp <- st_intersection(t, p)
tp <- st_transform(tp, area_proj(p))
tp$teow_intersect_sqkm <- st_area(tp)/1000000
myData <- as_tibble(tp)
myData_f <- myData %>% 
  select(WDPAID, BIOME_NAME, ECO_NAME, teow_intersect_sqkm)
paged_table(myData_f)

# Terrestrial Ecoregions of the World (supported wdpa)--------------------------

t <- st_read("/home/ombhandari/shared/datalake/mapme.protectedareas/input/teow/Terrestrial-Ecoregions-World.gpkg")
t <- st_make_valid(t)
p <- st_read("/home/ombhandari/shared/datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs.gpkg")
p <- st_transform(p, st_crs(t))
tp <- st_intersection(t, p)
tp <- st_transform(tp, area_proj(p))
tp$teow_intersect_sqkm <- st_area(tp)/1000000
myData <- as_tibble(tp)
myData_f <- myData %>% 
  select(WDPAID, BIOME_NAME, ECO_NAME, teow_intersect_sqkm)
paged_table(myData_f)


# Carbon Balance (supported wdpa)-----------------------------------------------

p <- st_read("/home/ombhandari/shared/datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs.gpkg")
p <- st_transform(p, "+proj=longlat +datum=WGS84 +no_defs")
pv <- vect(p)
myRaster <- rast("/home/rstudio/shared/datalake/mapme.protectedareas/input/net_carbon_flux/LA.tif")
myCrop <- terra::crop(myRaster, pv)
myMask <- terra::mask(myCrop, pv)
r <- terra::rasterize(pv, myMask, pv$WDPAID, background=NA, update=FALSE, touches=is.lines(pv), cover=FALSE)
zstats <- zonal(myMask, r, fun='sum', na.rm=T)
df.zstats <- data.frame(WDPAID=NA,
                        Net_Forest_Carbon_Flux=NA)
colnames(zstats) <- colnames(df.zstats)
rbind(df.zstats,zstats)[-1,]


# Carbon Balance (all wdpa) ----------------------------------------------------

p <- st_read("/home/ombhandari/shared/datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_allPAs.gpkg")
p <- st_transform(p, "+proj=longlat +datum=WGS84 +no_defs")
pv <- vect(p)
myRaster <- rast("/home/rstudio/shared/datalake/mapme.protectedareas/input/net_carbon_flux/LA.tif")
myCrop <- terra::crop(myRaster, pv)
myMask <- terra::mask(myCrop, pv)
r <- terra::rasterize(pv, myMask, pv$WDPAID, background=NA, update=FALSE, touches=is.lines(pv), cover=FALSE)
zstats <- zonal(myMask, r, fun='sum', na.rm=T)
df.zstats <- data.frame(WDPAID=NA,
                        Net_Forest_Carbon_Flux=NA)
colnames(zstats) <- colnames(df.zstats)
rbind(df.zstats,zstats)[-1,]