## Processing hurricane data for spatial impact evaluation
# Author: Johannes Schielein
# Last Edit: 2022-06-03

# call libraries
library(terra)
library(pbmcapply)
library(tidyverse)
library(mapview)
library(sf)
# set the temporary directory in order to avoid disk usage problems. 
terraOptions(tempdir="../../datalake/tempdir/")

# load input data
# AOIs - these AOIs contain polyid and CRS
aoi <-
  vect("../../Om/test/honeycomb_5_sqkm_subset_crs_polyid.gpkg")

# hurricane data
# note: the preprocessed data is not available online. It was created with an own cript
# Ths script is part of the repository available under "code/development/hurricanes.R"
hurricanes <-
  rast( "../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V03/max_likely_windspeed_H01_global_2000-2020.tif")

# test with subset
# mapView(st_as_sf(aoi[30000:45000,]))
aoi<-aoi[500001:nrow(aoi),]

starttime<-Sys.time()
# create routine to classify hurricane raster
lapply(2000:2020, function(i) {
  # 
  print(paste("Start processing year", i))

  # create index for year passed
  year_index <- data.frame(
    year = 2000:2020,
    index = 1:21
  )
  # extract index from year
  n <- subset(year_index,
    year == i,
    select = index
  )
  # retrieve hurricane raster for the year
  h <- hurricanes[[n[[1]]]]
  # get min and max values from raster
  v <- terra::minmax(h)
  # create matrix
  m <- c(
    NA, 0, 0,
    v[[1]] - 1, v[[2]] + 1, 1
  )
  # create a classification matrix
  rclmat <- matrix(m,
    ncol = 3,
    byrow = TRUE
  )
  # classify the raster using the matrix
  rc1 <- classify(
    h,
    rclmat
  )

  # create processing routine to get the area affected and not affected by hurricanes
  area_stats <- pbmclapply(1:nrow(aoi), function(j) {
    
    tryCatch({
      
      # crop the classified raster to the polygon
      crop <- terra::crop(
        rc1,
        aoi[j, ]
      )
      # mask the classified raster to the polygon
      mask <- terra::mask(
        crop,
        aoi[j, ]
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
      out$poly_id <- aoi[j, ]$poly_id
      # pivot to long format
      result <- tidyr::pivot_longer(out,
                                    cols = "area"
      )
      result
      },
      error = function(e) {
        message('Error in this line!')}
    )
  }, mc.cores = 10)
    # unlist the list as data frame
    final <- dplyr::bind_rows(area_stats)
    # write result to disk
    write.csv(
      final,
      paste0("../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/hurricane/honeycomb_5sqkm_hurricanes_2ndpart_", i, ".csv"),
      row.names = FALSE
    )
    # remove temp files
    tmpFiles(remove = T)
    # unlink(paste0(tempdir(),"/spat*"), recursive = T)
    # unlink(paste0(tempdir(),"/file*"), recursive = T)
    # message
    print(paste0("Done processing for year", i, sep=" "))
})
stoptime<-Sys.time()

starttime-stoptime
## NOTE:
# The scripts above process for all years 2000 to 2020 and saves the result yearly as csv file
