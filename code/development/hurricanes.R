## Processing routine for hurricane data
# Author: Johannes Schielein
# Last Edit: 2022-06-02


# download IBTracs data from arquive
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

# set the temporary directory in order to avoid disk usage problems. 
terraOptions(tempdir="../../datalake/tempdir/")

# load data 
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

# create a dataset for windspeeds above 64 knots (more or less the threashold for winds to cause forest disturbances)
hist(hurricanes_raw$wind_combinded)
table(hurricanes_raw$wind_combinded>64)/nrow(hurricanes_raw) # will contain only about 19% of all obs. 

hurricanes_subset<-
  hurricanes_raw %>% 
  filter(wind_combinded>64) %>% 
  filter(DIST2LAND<100)

# have a look at the data
# mapView(hurricanes_subset,zcol = "wind_combinded", at = seq(64, 185, 20), legend = TRUE)



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

# ---- regional subset (if desired) -----
# load data for the subset
# load wdpa with kfw areas
wdpa_LA<-
  st_read("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_valid_simplified.gpkg")

# load  countries
gadm_SA<-
  readRDS("../../datalake/mapme.protectedareas/input/gadm/gadm_3_6-2022-02-26/gadm36_adm0_r5_pk.rds")

gadm_SA<-terra::vect(gadm_SA)
gadm_SA<-st_as_sf(gadm_SA)

# subset countries
country_index<-
  which(gadm_SA$GID_0%in%unique(wdpa_LA$ISO3))

gadm_SA<-
  gadm_SA[country_index,]

# change gadm crs to intersect
gadm_SA<-
  st_transform(gadm_SA,crs = st_crs(hurricanes_subset))

# intersection hurricane line data with supported countries
intersection_results<-
  st_intersection(hurricanes_subset,
                  gadm_SA)

# mapView(intersection_results)+mapView(gadm_SA)
hurricanes_subset<-intersection_results

# ---- buffer values based un radii -----
# reproject
crs_robinson_world <-
  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

hurricanes_subset<-
  st_transform(hurricanes_subset,crs = crs_robinson_world)

# convert radius (currently miles) into meters
hurricanes_subset$R64_combined_model_meters<-
  hurricanes_subset$R64_combined_model*1609.34

# buffer layer with meters distance
hurricanes_subset_buf<-
  st_buffer(hurricanes_subset,
            dist = hurricanes_subset$R64_combined_model_meters,
            endCapStyle="ROUND"
  )

# ----- correct invalid geometries @ the dateline-----
# geometry errors come from features that cross the dateline at -180 degreee. 
# suppoesdly this can be fixed by using st_wrap_dateline() but for some reason it does not work on the data. 
# the proposed solution is to delete features (hurricane tracks) that cross the dateline and cannot be fixed. 

# project back again
hurricanes_subset_buf<-
  st_transform(hurricanes_subset_buf,crs = 4326)
# make as much as possible geoms valid
hurricanes_subset_buf<-
  st_make_valid(hurricanes_subset_buf)
# delete invalid geoms (if necessary for dateline wrapping)
# hurricanes_subset_buf <-
#   hurricanes_subset_buf[-which(st_is_valid(hurricanes_subset_buf) == F), ]

# wrap dateline geometries
hurricanes_subset_buf<- hurricanes_subset_buf%>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES"))

# project back again
hurricanes_subset_buf<-
  st_transform(hurricanes_subset_buf,crs = 4326)

mapView(hurricanes_subset,zcol = "wind_combinded", at = seq(64, 185, 20), legend = TRUE) + 
  mapView(hurricanes_subset_buf,zcol = "wind_combinded", at = seq(64, 185, 20), legend = TRUE)

# st_write(
#   hurricanes_subset_buf,
#   "../../datalake/mapme.protectedareas/processing/hurricanes/hurricanes_subset_buf.gpkg"
# )
# 
# hurricanes_subset_buf_reimport<-
#   st_read("../../datalake/mapme.protectedareas/processing/hurricanes/hurricanes_subset_buf.gpkg")

# ----- rasterize and take maximum -----
# proposed output data: maximum (likely) windspeed that affected a given region due to a storm of the category hurricane (at least H1 or 64 knts. windspeed)
# st_interpolate_aw(x, to, extensive=F)
aux_grid <-
  rast(vect(hurricanes_subset_buf), resolution = 0.001)

results_list <- as.list(vector(length=length(2000:2020)))

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
        hurricanes_aux%>%
          filter(SID == unique(.$SID)[i]) %>%
          arrange(desc(ISO_TIME)) %>% # arrange polygons by time for rasterization
          vect(.) %>%
          terra::rasterize(., aux_grid, 
                           field = "wind_combinded",
                           # filename= paste("../../datalake/mapme.protectedareas/processing/hurricanes/raster", i, "_", my_season, ".tif",sep = "")
                           )
        # remove tempfiles to avoid memory issues
        # tmpFiles(remove = T)
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
# dir.create("../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V03")

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
    paste("../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V03/max_likely_windspeed_H01_global_",i,".tif"),
    overwrite = T,
    datatype = "INT1U",
    memfrac = 0.8
  )
  # remove temporary files from terra package
  tmpFiles(remove = T)
}

# stack data and write out as a rater stack
rast_result_stack<-
  rast(list.files("../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V03/", full.names = TRUE, pattern=".tif"))

writeRaster(
  rast_result_stack,
  "../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V03/max_likely_windspeed_H01_global_2000-2020.tif",
  overwrite = T,
  datatype = "INT1U",
  memfrac = 0.8)

# # reduce to max from all years
# rast_result_stack_reduce <- app(rast_result_stack,
#                                 fun = "max", na.rm = T,
#                                 filename=  "../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V03/max_likely_windspeed_H01_global_2000-2020_reduced.tif",
#                                 overwrite=T)

# # export original line-data for display purposes
# write_sf(hurricanes_subset,
#          "../../datalake/mapme.protectedareas/processing/hurricanes/2000-2020_V03/max_likely_windspeed_H01_global_2000-2020_lines.gpkg")

# mapView(hurricanes_subset,zcol = "wind_combinded", at = seq(64, 185, 20), legend = TRUE) + 
#   mapView(rast_result_stack_reduce)

# ----- Analyse protected areas portfolio ----
## Desired Output Indicators:
# 1: zonal stats: max, min, mean, meadian
# 2: total affected surface area in ha. 



