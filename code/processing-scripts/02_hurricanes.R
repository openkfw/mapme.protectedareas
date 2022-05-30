## Processing hurricane data for spatial impact evaluation
# Author: Johannes Schielein
# Last Edit: 2022-05-30

# call libraries
library(terra)
library(pbmcapply)

# load input data
# AOIs - these AOIs contain polyid and CRS
aoi <-
  vect("../test/honeycomb_5_sqkm_subset_crs_polyid.gpkg")
# hurricane data
# note: the preprocessed data is not available online. It was created with an own cript
# Ths script is part of the repository available under "code/development/hurricanes.R"
# hurricanes <-
#   rast("../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V01/max_likely_windspeed_H01_global_2000-2020.tif")
# since the hurricane raster contains 21 raster layers from years 2000-2020
# lets get raster layer for year 2000 first
# l1 <- hurricanes[[1]]
# the raster values range from 64 to 120 - which indicates the speed and also NA values denoting no hurricane at all
# classify the hurricane raster into two classes 0 and 1 - 0 for NAs and 1 for value range (64-120)
# m <- c(
#   NA, 0, 0,
#   62, 153, 1
# )
# # create a classification matrix
# rclmat <- matrix(m,
#   ncol = 3,
#   byrow = TRUE
# )
# # classify the raster using the matrix
# classified_raster <- classify(
#   l1,
#   rclmat
# )

## Updates:
# The hurricane rasters are already classified and saved as tif and the resolution has been rescaled to higher pixel values.
classified_raster <- 
  rast("../test/hurricane_classified/cropped/classified_hurricane_2000.tif")

# create processing routine to get the area affected and not affected by hurricanes
hurricane_stats <- pbmclapply(1:nrow(aoi), function(i) {

  # crop the classified raster to the polygon
  crop <- terra::crop(
    classified_raster,
    aoi[i, ]
  )
  # mask the classified raster to the polygon
  mask <- terra::mask(
    crop,
    aoi[i, ]
  )
  # compute area raster
  area_raster <- cellSize(mask,
    unit = "km"
  )
  # create patchsizes using zonal stats
  patchsizes <- zonal(
    area_raster,
    mask,
    sum
  )
  # rename the class
  class_name <- data.frame(
    max = c(0, 1),
    classes = c("not_affected", "affected")
  )
  # merge patchsizes and class name data frame
  out <- merge(
    x = patchsizes, y = class_name,
    by.x = colnames(patchsizes)[1],
    by.y = colnames(class_name)[1]
  )
  out <- out[, -1]
  # add polygon id to the result
  out$poly_id <- aoi[i, ]$poly_id
  # pivot to long format
  result <- tidyr::pivot_longer(out,
    cols = "area"
  )
  result
}, mc.cores = 8)

# unlist the list as data frame
final <- dplyr::bind_rows(hurricane_stats)

# NOTE:
# The scripts above process only for year 2000.
# To process for other years (say 2001), replace the file name in line `41` to **classified_hurricane_2001.tif**
# similarly, for year 2020, it will be **classified_hurricane_2020.tif**

# directory to save the results
# saveRDS(data, "../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/honeycomb_5sqkm_hurricanes.rds")
