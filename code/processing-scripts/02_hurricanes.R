## Processing hurricane data for spatial impact evaluation
# Author: Johannes Schielein
# Last Edit: 2022-05-23

# call libraries
library(terra)
library(sf)
library(mapview)

# load input data
# AOIs
aoi <-
  st_read(
    "/datadrive/datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset.gpkg"
  )


# create poly_id
aoi$poly_id<-
  1:nrow(aoi)

# hurricane data
  # note: the preprocessed data is not available online. It was created with an own cript 
  # Ths script is part of the repository available under "code/development/hurricanes.R"  
hurricanes<-
  rast("../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V01/max_likely_windspeed_H01_global_2000-2020.tif")

# calculate area of maximum windspeed above 64 knots 

# create testsubset in central america
aoi_subset<-aoi[420000:421000,]
mapView(aoi_subset)

# 
hurricanes
# check CRS
st_crs(aoi_subset)

# reproject to match CRS
aoi_subset <- 
  st_transform(aoi_subset, crs = 4326)

# convert to terra zonal format
aoi_subset <- 
  vect(aoi_subset)

#
test_extract <-
  terra::extract(hurricanes,
                 aoi_subset)

table(test_extract[,2])


saveRDS(data, "../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/honeycomb_5sqkm_hurricanes.rds")
