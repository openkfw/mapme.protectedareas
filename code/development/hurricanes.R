## Processing routine for hurricane data
# download data from arquive

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


# call data 
hurricanes_raw <-
  read_sf(
    "../../datalake/mapme.protectedareas/input/hurricanes/2022-02-21/IBTrACS.since1980.list.v04r00.lines/IBTrACS.since1980.list.v04r00.lines.shp"
  )

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

# ----- create a dataset for windspeeds above 64 knots ----
hist(hurricanes_raw$wind_combinded)
table(hurricanes_raw$wind_combinded>64)/nrow(hurricanes_raw) # will contain only about 19% of all obs. 

hurricanes_subset<-
  hurricanes_raw %>% 
  filter(wind_combinded>64) %>% 
  filter(DIST2LAND<100)

# have a look at the data
mapView(hurricanes_subset,zcol = "wind_combinded", at = seq(64, 185, 20), legend = TRUE)

# ---- get parameter estimation of 64 knts influence radius ----
hurricanes_subset$R64_combined <-
  hurricanes_subset %>%
  select(contains("R64")) %>%
  st_drop_geometry() %>%
  rowMeans(., na.rm = T)

# look at correleation visually
hurricanes_subset %>% 
  ggplot() + 
  geom_point(aes(wind_combinded, R64_combined))

# create linear model
hurricanes_model<-
hurricanes_subset %>% 
  lm(R64_combined~wind_combinded,data = .)

# predict based on real data 
hurricanes_subset$R64_modeled<-
  predict.lm(hurricanes_model,hurricanes_subset)

# compare predicted to real
hurricanes_subset %>% 
  select(R64_combined,R64_modeled) %>% 
  st_drop_geometry() %>% 
  View()

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

# project layer 
hurricanes_subset <- 
  st_transform(hurricanes_subset, crs = 3395)

hurricanes_subset<-st_make_valid(hurricanes_subset)

# buffer layer with meters   distance
hurricanes_subset_buf<-
  st_buffer(hurricanes_subset,
            dist = hurricanes_subset$R64_combined_model_meters,
            endCapStyle="ROUND"
            )

# ----- rasterize and take maximum -----
# proposed output data: maximum (likely) windspeed that affected a given region due to a storm of the category hurricane (at least H1 or 64 knts. windspeed)
# st_interpolate_aw(x, to, extensive=F)

aux_grid <-
  rast(vect(hurricanes_subset_buf), resolution = 1000)

results_list<-as.list(vector(length=length(2000:2020)))

f_rast_calc <- function(my_season)
{# subset data for season
  hurricanes_aux <-
    hurricanes_subset_buf %>%
    filter(SEASON == my_season)
  # get
  k <- length(unique(hurricanes_aux$SID))
  aux_list <- as.list(vector(length = k))
  # rasterize each polygon feature separatly, stack rasters and the reduce to a single raster taking the maximum 
  for (i in 1:k) {
    aux_list[[i]] <-
    tryCatch({
      # filter dataset, rearrange, convert to terra vector format and rasterize. 
        hurricanes_aux %>%
        filter(SID == unique(.$SID)[i]) %>%
        arrange(ISO_TIME) %>% # arrange polygons by time for rasterization
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
# test function
test<-f_rast_calc(2000)

writeRaster(test,"../../datalake/mapme.protectedareas/processing/hurricanes/test/test.tif")

test<-rast("../../datalake/mapme.protectedareas/processing/hurricanes/test/test.tif")

# test2<-terra::max(test,na.rm=T)
test2<-app(test,fun="mean")



?max 
# apply funnction through all seasons
# for (p in 2000:2020) {
#   writeRaster(
#     f_rast_calc(p),
#     paste(
#       "../../datalake/mapme.protectedareas/processing/hurricanes/test/hurricane_maxspeeds_",
#       p,
#       ".tif",
#       sep = ""
#     ),
#     overwrite = T,
#     datatype = "INT1U",
#     memfrac = 0.8
#   )
# }




###### ARQUIVW
# ----- delete faulty entries -----
# comment: geometry errors are due to some observations in the eastern pacific. Since I do not know the actual cause of the problem I just delte them from 
# the dataset by identification via their large surface areas. 

# hurricanes_subset_buf %>%
#   #filter(LON>170&LON<180) %>%
#   mapView(., zcol = "LON", legend = TRUE)
# st_bbox_by_feature = function(x) {
#   x = st_geometry(x)
#   f <- function(y) st_as_sfc(st_bbox(y))
#   do.call("c", lapply(x, f))
# }
# 
# test<-
#   st_bbox_by_feature(hurricanes_subset_buf)
# # xmin, ymin, xmax and ymax 
# colnames(as.tibble(test))
# plot(st_geometry(hurricanes_subset_buf))
# 
# hurricanes_subset_buf$area_aux<-
#   st_area(hurricanes_subset_buf)
# 
# hurricanes_subset_buf$area_aux<-
#   units::set_units(hurricanes_subset_buf$area_aux, km^2)
# 
# sort(hurricanes_subset_buf$area_aux,decreasing = T)
#  
# hurricanes_subset_buf <-
#   hurricanes_subset_buf %>% 
#   filter(as.numeric(area_aux)<10^6)
# 
# mapView(hurricanes_subset_buf,zcol = "area_aux", legend = TRUE)



