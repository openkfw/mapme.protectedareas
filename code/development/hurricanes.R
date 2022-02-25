## Processing routine for hurricane data
# download data from arquive

# dir.create("../../datalake/mapme.protectedareas/input/hurricanes/2022-02-21/")
# download.file(
#   "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/shapefile/IBTrACS.since1980.list.v04r00.lines.zip",
#   "../../datalake/mapme.protectedareas/input/hurricanes/2022-02-21/IBTrACS.since1980.list.v04r00.lines.zip"
# )
# 
# unzip(zipfile = "../../datalake/mapme.protectedareas/input/hurricanes/2022-02-21/IBTrACS.since1980.list.v04r00.lines.zip",
#       exdir = "../../datalake/mapme.protectedareas/input/hurricanes/2022-02-21/IBTrACS.since1980.list.v04r00.lines")

# call libs
library("sf")
library("terra")
library("dplyr")
library("mapview")
library("ggplot2")
source("code/area_proj.R")


# call data 
hurricanes_raw <-
  read_sf(
    "../../datalake/mapme.protectedareas/input/hurricanes/2022-02-21/IBTrACS.since1980.list.v04r00.lines/IBTrACS.since1980.list.v04r00.lines.shp"
  )

# ----- filter data ---- 
# column description is given here: https://www.ncei.noaa.gov/sites/default/files/2021-07/IBTrACS_v04_column_documentation.pdf
# subset data for observational period (2000-2020)
hurricanes_raw$ISO_DATE <- as.Date(hurricanes_raw$ISO_TIME,
                                   format = "%Y-%m-%d")

# filter for relevant time-period only. 
hurricanes_raw <-
  hurricanes_raw %>%
  filter(ISO_DATE >=  "2000-01-01")

# create column with windspeed information which contains averages reported from different organizations
hurricanes_raw$wind_combinded <-
  hurricanes_raw %>%
  select(contains("WIND")) %>%
  select(-WMO_WIND) %>%
  st_drop_geometry() %>%
  rowMeans(., na.rm = T)

# count how many have no observations
table(is.na(hurricanes_raw$wind_combinded))/nrow(hurricanes_raw) # about 3% that is acceptable

# filter out those that have not windspeed information
hurricanes_raw<-
  hurricanes_raw %>% 
  filter(!is.na(wind_combinded))

# create a dataset for windspeeds above 64 knots
hist(hurricanes_raw$wind_combinded)
table(hurricanes_raw$wind_combinded>64)/nrow(hurricanes_raw) # will contain only about 19% of all obs. 

hurricanes_subset<-
  hurricanes_raw %>% 
  filter(wind_combinded>64) %>% 
  filter(DIST2LAND<100)

# have a look at the data
mapView(hurricanes_subset,zcol = "wind_combinded", at = seq(64, 185, 20), legend = TRUE)

# ----- delete data with invalid geometries -----
# comment: geometry errors are due to some observations in the eastern pacific. They form long stretches of lines over the whole globe.
# the problem only becomes relevant when reprojecting the data. Reprojection though is required to later buffer based on metrical units 
# Because the reason for these problems is unknown, we simply filter  out falty geometries abfter projection based on the feature length. 
# for the data after 2000 this problem only appeared in four observations.
crs_robinson_world<-
"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

plot(hurricanes_subset["LAT"])

hurricanes_subset<-
  st_transform(hurricanes_subset,crs = crs_robinson_world)

# plot(hurricanes_subset["LAT"])

hurricanes_subset$length<-
  st_length(hurricanes_subset)

hurricanes_subset$length<-
  units::set_units(hurricanes_subset$length, km)

head(sort(hurricanes_subset$length,decreasing=T))
# mapView(hurricanes_subset,zcol = "wind_combinded", legend = TRUE)

# filter out those where the length is more then  30000km. Note. This might change if projection system is changed. 
hurricanes_subset<- hurricanes_subset %>% 
  filter(as.integer(length)<30000)

# plot(hurricanes_subset["LAT"])
# ---- create a combined 64 knots radius estimation for buffering ----
hurricanes_subset$R64_combined <-
  hurricanes_subset %>%
  select(contains("R64")) %>%
  st_drop_geometry() %>%
  rowMeans(., na.rm = T)

# create linear model
hurricanes_model<-
hurricanes_subset %>% 
  lm(R64_combined~wind_combinded,data = .)

# predict based on real data 
hurricanes_subset$R64_modeled<-
  predict.lm(hurricanes_model,hurricanes_subset)

# substitute missings with predicted
hurricanes_subset$R64_combined_model <-
  ifelse(
    is.nan(hurricanes_subset$R64_combined) == T,
    hurricanes_subset$R64_modeled,
    hurricanes_subset$R64_combined
  )

# ---- buffer values based un radii -----
# convert radius (currently miles) into meters
hurricanes_subset$R64_combined_model_meters<-
  hurricanes_subset$R64_combined_model*1609.34

# buffer layer with meters   distance
hurricanes_subset_buf<-
  st_buffer(hurricanes_subset,
            dist = hurricanes_subset$R64_combined_model_meters,
            endCapStyle="ROUND"
            )

mapView(hurricanes_subset,zcol = "wind_combinded", at = seq(64, 185, 20), legend = TRUE) + 
  mapView(hurricanes_subset_buf,zcol = "wind_combinded", at = seq(64, 185, 20), legend = TRUE)
# ----- rasterize and take maximum -----
# proposed output data: maximum (likely) windspeed that affected a given region due to a storm of the category hurricane (at least H1 or 64 knts. windspeed)
# st_interpolate_aw(x, to, extensive=F)
aux_grid <-
  rast(vect(hurricanes_subset_buf), resolution = 5000)

results_list<-as.list(vector(length=length(2000:2020)))

f_rast_calc <- function(my_season)
{# subset data for season
  hurricanes_aux <-
    hurricanes_subset_buf %>%
    filter(SEASON == my_season)
  # get
  k <- length(unique(hurricanes_aux$SID))
  aux_list <- as.list(vector(length = k))
  # rasterize each polygon feature separately, stack rasters and the reduce to a single raster taking the maximum 
  for (i in 1:k) {
    aux_list[[i]] <-
    tryCatch({
      # filter dataset, rearrange, convert to terra vector format and rasterize. 
        hurricanes_aux %>%
        filter(SID == unique(.$SID)[i]) %>%
        arrange(desc(ISO_TIME)) %>% # arrange polygons by time for rasterization
        vect %>%
        terra::rasterize(., aux_grid, field = "wind_combinded")
    }, error = function(e){
      print("Problem with data. Please check.") 
      return(NA)})
    print(paste("Done with", i, "out of", k, "from year", my_season, sep = " "))

  }
  # create raster stack and then reduce to single layer again by taking the maximum values. 
  rast(unlist(aux_list)) #%>% 
   # max(.,na.rm=T)
  
}

## Apply function for all years
# dir.create("../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V01")

for(i in 2000:2020) {
  # test function
  rast_result <-
    f_rast_calc(i)
  
  # project result back into WGS84
  rast_result <-
    project(rast_result, "epsg:4326")
  
  # get maximum value for year
  rast_result_reduce <- app(rast_result,
                            fun = "max", na.rm = T)
  
  # store result
  # dir.create("../../datalake/mapme.protectedareas/processing/hurricanes/test/")
  writeRaster(
    rast_result_reduce,
    paste("../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V01/max_likely_windspeed_H01_global_",i,".tif"),
    overwrite = T,
    datatype = "INT1U",
    memfrac = 0.8
  )
}

# stack data and write out as a rater stack
rast_result_stack<-
  rast(list.files("../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V01/", full.names = TRUE))

writeRaster(
  rast_result_stack,
  "../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V01/max_likely_windspeed_H01_global_2000-2020.tif",
  overwrite = T,
  datatype = "INT1U",
  memfrac = 0.8)

# reduce to max from all years
rast_result_stack_reduce <- app(rast_result_stack,
                          fun = "max", na.rm = T)

writeRaster(
  rast_result_stack_reduce,
  "../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V01/max_likely_windspeed_H01_global_2000-2020_reduced.tif",
  overwrite = T,
  datatype = "INT1U",
  memfrac = 0.8)

# export original line-data for display purposes
write_sf(hurricanes_subset,
         "../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V01/max_likely_windspeed_H01_global_2000-2020_lines.gpkg")

mapView(hurricanes_subset,zcol = "wind_combinded", at = seq(64, 185, 20), legend = TRUE) + 
  mapView(rast_result_stack_reduce)

# ----- Analyse protected areas portfolio ----
## Desired Output Indicators:
# 1: zonal stats: max, min, mean, meadian
# 2: total affected surface area in ha. 


# ----- ARQUIVE -----

## TEST AND APPLY RASTERIZATION FUNCTION
# # test function
# rast_result <-
#   f_rast_calc(2012)
# 
# # project result back into WGS84
# rast_result <-
#   project(rast_result, "epsg:4326")
# 
# # get maximum value for year
# rast_result_reduce <- app(rast_result,
#                           fun = "max", na.rm = T)
# 
# # store result
# # dir.create("../../datalake/mapme.protectedareas/processing/hurricanes/test/")
# writeRaster(
#   rast_result_reduce,
#   "../../datalake/mapme.protectedareas/processing/hurricanes/test/test5.tif",
#   overwrite = T,
#   datatype = "INT1U",
#   memfrac = 0.8
# )


