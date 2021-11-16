# routines to create buffer around Protected Areas (PAs) ------------------------
## NOTE: The routine below saves only the buffer zones (excluding PA areas)

# load required libraries to follow this routine
library("sf")
library("tidyr")
library("tidyverse")
library("parallel")
library("pbmcapply")

# What are the arguments needed?

# (1) pa_polygon = polygon on which to create buffer
# (2) buffsize = buffer distance (in meter)
# (3) area = area in sqkm for subset (remove polygons less than provided area)
# (4) file = filepath to save buffer object eg. ("../")
# (5) n = no. of cores


# create a function to create buffer aroud PA polygons
create_buffer <- function(pa_polygon, buffsize, area, filepath, n) {
  
  tryCatch(
    {
      
      # load polygon and transfom it to WGS84
      p_raw <- 
        st_transform(pa_polygon,
                     "+proj=longlat +datum=WGS84 +no_defs")
      print("pa polygon loaded")
      # subset polygon (subset object)
      p_sub <- 
        subset(p_raw,
               p_raw$AREA_KM2 > area)
      print("pa polygon subsetted")
      # create buffer (buffer object)
      p_buff <-
        st_buffer(p_sub, buffsize)
      print("buffer created")
      # compute difference (buffer - subset)
      diff = pbmclapply(1:nrow(p_sub), function(i) {
        
        d <- st_difference(p_buff[i, ], p_sub[i, ])
        return(d)
      }, mc.cores = n)
      print("pbmclapply done")
      # unlist to single sf object
      sf <- 
        do.call(rbind, diff)
      print("unlist to sf done")
      # write sf object to file
      write_sf(sf,
               paste0(filepath,
                      "buffer_object.gpkg"))
      print("sf object written to path successfully")
    },
    error = function(e) {
      message('Error! Please re-check your arguments and libraries!')
    }
  )
}

# # call function - test example
# create_buffer(pa_polygon = pa_polygon,
#               buffsize = 10000,
#               area = 1,
#               filepath = "../buffer/",
#               n = 6)
