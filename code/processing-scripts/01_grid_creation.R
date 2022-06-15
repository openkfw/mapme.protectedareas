## script to create grids  for the geospatial impact evaluation
# Author: Johannes Schielein
# Originally Created: 2022-02-27
# Last Update: 2022-03-31

## Important Note: This script requires at least about 40GB of RAM to work properly.
# Otherwise the Rsession unexpetedly crashes when creating a very large grid of currently > 3Mio features. 

# ----- call libraries -----
library("sf")
library("mapview")
library("units")
library("dplyr")
library("readr")

# ---- create honeycomb grid -----
# load wdpa data for KfW supported countries, project with EA projection and get bbox for grid
wdpa_kfw<-
  read_sf("data/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
source("code/area_proj.R")
wdpa_kfw<-
  st_transform(wdpa_kfw,crs = area_proj(wdpa_kfw))
bbox = st_as_sf(st_as_sfc(st_bbox(wdpa_kfw)))
# mapview(bbox)
# expected area can be specified as i in sqkm

dir.create("../../datalake/mapme.protectedareas/processing/fishnet")
# create honycomb grids for different gridcell sizes () 
for (i in c(5)) { # gridcell size in square kilometers (=100 ha). 10, 2, 1
  cellarea <- i * (1e+6)
  cellsize <- 2 * sqrt(cellarea / ((3 * sqrt(3) / 2))) * sqrt(3) / 2
  hexa <- st_make_grid(x = bbox,
                       cellsize = cellsize,
                       square = FALSE)
  st_write(hexa,
           paste(
             "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_",
             i,
             "_sqkm.gpkg",
             sep = "",
             append= FALSE # overwrite layer if exists. Use with caution
           )
  )
}

# ----- subset grid for terrestrial areas only -----
# reading the 5km data requires about 18 GB of RAM. If RAM is too low, rsession will crash as a result. 
honeycomb<-
  st_read("../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm.gpkg")

# load in countries
dir.create("../../datalake/mapme.protectedareas/input/gadm/gadm_3_6-2022-02-26")
gadm_world <-
  geodata::world(resolution = 5,
                 level = 0,
                 path = "../../datalake/mapme.protectedareas/input/gadm/gadm_3_6-2022-02-26")

gadm_world<-
  st_as_sf(gadm_world)
# subset countries (somehow dplyr syntax does not work)
country_index<-
  which(gadm_world$GID_0%in%unique(wdpa_kfw$ISO3))

gadm_SA<-
  gadm_world[country_index,]

# write_sf(gadm_SA, 
#          "../../datalake/mapme.protectedareas/input/gadm/gadm_3_6-2022-02-26/gadm_SA.gpkg")

# remove no further required objects
rm(gadm_world,country_index)

# change gadm crs to intersect
gadm_SA<-
  st_transform(gadm_SA,crs = st_crs(honeycomb))

# intersect
intersection_results <- 
  st_intersects(gadm_SA, honeycomb)

# unlist results
intersection_results<-
  unlist(intersection_results)

# subset honeycomb grid for intersected polygons
honeycomb<-
  honeycomb[sort(intersection_results),]

# save results
write_sf(honeycomb,
         "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset.gpkg",
         append= FALSE # overwrite layer if exists. Use with caution
         )

# ----- get intersection, within and boarder polygons of grid with PAs and buffered PAs -----
honeycomb<-
  read_sf( "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset.gpkg")

wdpa_LA<-
  st_read("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_valid_simplified.gpkg")

wdpa_LA <-
  wdpa_LA %>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") %>%
  filter(STATUS != "Proposed") %>%
  filter(GEOMETRY_TYPE != "POINT")

# change wdpa crs to intersect
wdpa_LA<-
  st_transform(wdpa_LA,crs = st_crs(honeycomb))

# make valid
wdpa_LA<-
  st_make_valid(wdpa_LA)

# subset for treated
wdpa_kfw_treated <-
  wdpa_LA %>%
  filter(!is.na(bmz_n_1))

## important to note that only 356 areas remain after filtering from 398

# buffer with a given distance
bufferdistance<-50000 # 50km
wdpa_kfw_treated_buf <- wdpa_kfw_treated %>%
  st_buffer(., bufferdistance) %>%
  st_union() %>% # unite to a geometry object
  st_sf() # make the geometry a data frame object

# make valid
wdpa_kfw_treated_buf <- 
  st_make_valid(wdpa_kfw_treated_buf)

## intersections
# intersection with all PAs
intersection_results <- 
  st_intersects(honeycomb,wdpa_LA,sparse = T)

# within with all PAs
within_results <- 
  st_within(honeycomb,wdpa_LA,sparse = T)

# within for buffered treatment PAs
within_buff_results <- 
  st_within(honeycomb,wdpa_kfw_treated_buf,sparse = T)

# create longtable dfs
intersection_results_df<-
  data.frame(row.id=rep(seq_along(intersection_results), 
                        lengths(intersection_results)), 
             col.id=unlist(intersection_results))

within_results_df<- # maybe change to st_contains()
  data.frame(row.id=rep(seq_along(within_results), 
                        lengths(within_results)), 
             col.id=unlist(within_results))

within_buff_results_df<- # maybe change to st_contains()
  data.frame(row.id=rep(seq_along(within_buff_results), 
                        lengths(within_buff_results)), 
             col.id=unlist(within_buff_results))

## add wdpaids for intersection and within
# add wdpa ids to it
intersection_results_df$wdpa_id<-
  st_drop_geometry(wdpa_LA)[intersection_results_df$col.id,"WDPAID"]

# add wdpa ids to it
within_results_df$wdpa_id<-
  st_drop_geometry(wdpa_LA)[within_results_df$col.id,"WDPAID"]


# change column names 
colnames(intersection_results_df)<-c("poly_id","wdpa_rowname","WDPAID")
colnames(within_results_df)<-c("poly_id","wdpa_rowname","WDPAID")
colnames(within_buff_results_df)<-c("poly_id","wdpa_rowname")


nrow(within_results_df)

## create df with polygons that cross boarder
# boarder polygons = intersection - within
boarder_results_df<-
  intersection_results_df %>% 
  filter(poly_id%in%within_results_df$poly_id==F)

##! 
## create df for controls (not within buffered regions and other PAS)
controls_df<-
  st_drop_geometry(honeycomb) %>% 
  filter(!row.id%in%intersection_results_df$poly_id) %>% 
  filter(!row.id%in%within_buff_results_df$poly_id)

colnames(controls_df)<-c("poly_id","wdpa_rowname")
##!

# save tables
write_csv(intersection_results_df,
          "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_intersect_wdpa_long.csv")

write_csv(within_results_df,
          "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_within_wdpa_long.csv")

write_csv(within_buff_results_df,
          "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_within_treated_buff50km_long.csv")

write_csv(boarder_results_df,
          "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_boarder_wdpa_long.csv")

write_csv(controls_df,
          "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_controlls.csv")


# ----- add intersection information to original honeycomb grid ----- 
# eventually read in data if already processed
# intersection_results_df <-
#   read.csv(
#     "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_intersect_wdpa_long.csv"
#   )
# within_results_df <-
#   read.csv(
#     "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_within_wdpa_long.csv"
#   )
# boarder_results_df <-
#   read.csv(
#     "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_boarder_wdpa_long.csv"
#   )

# create column for honeycomb to see whether it intersects with PAs
honeycomb$PA_intersect<-
  1:nrow(honeycomb)%in%intersection_results_df$poly_id

honeycomb$PA_within<-
  1:nrow(honeycomb)%in%within_results_df$poly_id

honeycomb$PA_boarder<-
  1:nrow(honeycomb)%in%boarder_results_df$poly_id

honeycomb$PA_within_buff50km<-
  1:nrow(honeycomb)%in%within_buff_results_df$poly_id

honeycomb$controls<-
  1:nrow(honeycomb)%in%controls_df$poly_id

honeycomb <-
  honeycomb %>%
  relocate(geom, .after = last_col())

# sf is currently not able to efficiently store Logicals. A conversion to 1/0 fixes this
honeycomb$PA_intersect<-
  ifelse(honeycomb$PA_intersect==T,1,0)

honeycomb$PA_within<-
  ifelse(honeycomb$PA_within==T,1,0)

honeycomb$PA_boarder<-
  ifelse(honeycomb$PA_boarder==T,1,0)

honeycomb$PA_within_buff50km<-
  ifelse(honeycomb$PA_within_buff50km==T,1,0)

# write spatial data out
write_sf(honeycomb,
         "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_intersect.gpkg")



# ----- create country codes from polygons -----
# load data if not loaded
honeycomb<-
  st_read("../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset.gpkg")

# load  countries
gadm_SA<-
  readRDS("../../datalake/mapme.protectedareas/input/gadm/gadm_3_6-2022-02-26/gadm36_adm0_r5_pk.rds")


gadm_SA<-terra::vect(gadm_SA)
gadm_SA<-st_as_sf(gadm_SA)

# subset countries
country_index<-
  which(gadm_SA$GID_0%in%unique(wdpa_kfw$ISO3))

gadm_SA<-
  gadm_SA[country_index,]


# change gadm crs to intersect
gadm_SA<-
  st_transform(gadm_SA,crs = st_crs(honeycomb))

# get centroids to facilitate intersections
honeycomb_centroids <- 
  st_centroid(honeycomb)

# create poly_ids
honeycomb_centroids$poly_id<-1:nrow(honeycomb_centroids)

# intersection polygons and centroids
intersection_results<- 
  st_intersection(honeycomb_centroids,
                  gadm_SA)

# merge with old data to get all rows and drop geometry
intersection_results_all<-
  merge(st_drop_geometry(honeycomb_centroids),st_drop_geometry(intersection_results),by="poly_id",all=T)
# table(is.na(intersection_results_all$GID_0)) # 12930 have no country value

# save output
write_csv(intersection_results_all,
          "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_countries.csv")

