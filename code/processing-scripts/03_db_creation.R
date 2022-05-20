# script to create the database for the matching process (matching frames)
# Goal: Matching frames should be created on an annual base. Each matching frame should contain year-specific matching variables...
  # ... for treatment and controls cells (based on KfWs project documentation) 

# author: Johannes Schielein 
# last modification: 2022-04-11

# call relevant libs. 
library("sf")
library("tidyverse")
library("mapview")
options(scipen =999) # disable sicentific notation


# ----- (1) Load and combine KfW input data-----
# load and process project data
project.data<-
  readxl::read_xlsx("../../datalake/mapme.protectedareas/input/kfw_finance/Portfolio_Auszahlungen.xlsx", skip = 4)

project.data.reduced<-
  read.csv("../../datalake/mapme.protectedareas/input/kfw_finance/mapme.protectedareas_kfw-finance-2021-03-17.csv")

## note: the scripts for creating the reduced project data are on private PC

# join
names(project.data)[5]<-"bmz_nummer"

project.data.reduced<-
  left_join(project.data.reduced,
            project.data,by="bmz_nummer")

# remove those projects, where the first disbursement was made before 2000
projectsbefore<-
  project.data.reduced$bmz_nummer[which(project.data.reduced$first_year<2000)]

# see how many projects area affected by this
unique(projectsbefore) #  3 projects are kicked out. 

project.data.reduced<-
  project.data.reduced%>%
  filter(!bmz_nummer%in%projectsbefore)


# ----- (2) get WDPA IDs for grids  -----
# get kfw treated polygons
wdpa_kfw<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

wdpa_kfw <-
  wdpa_kfw %>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") %>%
  filter(STATUS != "Proposed") %>%
  filter(GEOMETRY_TYPE != "POINT")


# transform bmz table to long
wdpa_kfw_long <-
  wdpa_kfw %>%
  st_drop_geometry() %>%
  select(WDPAID, starts_with("bmz_n")) %>%
  pivot_longer(., cols = starts_with("bmz_n"), ) %>%
  filter(!is.na(value))

# get intersection ids
grid_intersection <-
  read.csv(
    "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_intersect_wdpa_long.csv"
  )

grid_intersection$treated <-
  ifelse(grid_intersection$WDPAID %in% wdpa_kfw_long$WDPAID, 1, 0)


# ---- (3) Match Project Start and End Information to the sampling AOIs -----
## write a function to create columns that will indicate whether data should be sampled for a specific year or not. 
f.matchingyear <- function(matching.year) {
  # get relevant bmz number for year
  bmz_number <-
    project.data.reduced %>%
    filter(first_year == matching.year) %>% # note that we use the first year of payments here. Not the contract date.
    pull(bmz_nummer)
  # get relevant WDPA ids from all possible bmz projects.
  matching.ids <-
    wdpa_kfw_long %>%
    filter(value %in% bmz_number)%>%
    pull(WDPAID)
  # get relevant unique ids from sampling frame
  grid_intersection.filter <-
    grid_intersection %>%
    filter(WDPAID %in% matching.ids) %>%
    mutate(matching_year = matching.year)
  
  #  write into the AOI Frame the sampling year
  return(grid_intersection.filter)
}

# apply the function for all years
matching.frame<-do.call(rbind,lapply(2000:2020,f.matchingyear))
# table(matching.frame$matching_year)
# head(matching.frame)

# ----- (4) Load matching variables, clean data, create additional variables and merge final results-----
## 4.1 Forest area and emisssions
  ## what is needed for matching frame: Forest area + forest cover loss before treatment (t-1 till t-3)
# load data
input_gfw<-
  readRDS("../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/honeycomb_5sqkm_treeloss.rds")
# drop the geometry
input_gfw <- st_drop_geometry(input_gfw)
# unnest the relevant indicators
input_gfw <-
  input_gfw %>% unnest(treeloss)

# create longtable
input_gfw_wide <-
  input_gfw %>%
  pivot_wider(names_from = years,
              values_from = c(treecover, emissions))

# create lossdata
lossdata <- 
  abs(input_gfw_wide[3:22] - input_gfw_wide[(3:22) - 1])

colnames(lossdata) <- 
  gsub("treecover", "loss", colnames(lossdata))

# create lossdata for t-1 till t-3
input_gfw_wide <- cbind(input_gfw_wide, lossdata)

# create lossdata for t-3
lossdata_t3<- lossdata[(4:20) - 1] + lossdata[(4:20) - 2] +  lossdata[(4:20) - 3]
colnames(lossdata_t3)<-paste("loss_t3_",2004:2020,sep="")

input_gfw_wide <- cbind(input_gfw_wide, lossdata_t3)
colnames(input_gfw_wide)

rm(input_gfw)
# save.image("../../datalake/mapme.protectedareas/output/matching/matching_frames/full_database_2022-03-31.Rdata")

## 4.2 Accessibility
# needed for matching frames: minimum Accessibility 5k_110mio (currently only mean)
input_accessibility<-
  readRDS("../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/honeycomb_5sqkm_accessibility.rds")

# drop the geometry
input_accessibility <- st_drop_geometry(input_accessibility)
# unnest the relevant indicators
input_accessibility <-
  input_accessibility %>% unnest(accessibility)

# create longtable
input_accessibility_wide <-
  input_accessibility %>%
  select(.assetid,minutes_mean,distance) %>% 
  pivot_wider(names_from = distance,
              values_from = minutes_mean)

colnames(input_accessibility_wide)[-1]<-paste("traveltime",colnames(input_accessibility_wide)[-1],sep="_")
rm(input_accessibility)

## 4.3 Elevation and TRI 
input_srtm<-
  readRDS("../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/honeycomb_5sqkm_srtm.rds")
# drop the geometry
input_srtm <- st_drop_geometry(input_srtm)
# unnest the relevant indicators
input_srtm <-
  input_srtm %>% unnest(c(tri,elevation))

colnames(input_srtm)

# needed for matching Frame: Elevation and TRI 
input_srtm_wide <-
  input_srtm %>%
  select(.assetid,terrain_ruggedness_index_mean,elevation_mean)

rm(input_srtm)

## 4.4 Soil characteristics
input_soils<-
  readRDS("../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/honeycomb_5sqkm_soil_merged.rds")

# drop the geometry
input_soils <- st_drop_geometry(input_soils)

# unnest the relevant indicators
input_soils <-
  input_soils %>% unnest(soilproperties)


# transform to wide
input_soils_wide <-
  input_soils %>%
  filter(depth=="5-15cm") %>% 
  select(.assetid,layer,mean) %>% 
  pivot_wider(names_from = layer,
              values_from = mean)

colnames(input_soils_wide)[-1]<-paste("soil_5_15cm_",colnames(input_soils_wide)[-1],sep="")
rm(input_soils)

## Countries 
input_countries_wide <-
  read.csv(
    "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_countries.csv"
  )
colnames(input_countries_wide)[1]<-colnames(input_srtm_wide)[1]

# needed for matching Frame: Country names


## merge input data
# put all data frames into list
df_list <- list(input_gfw_wide, 
                input_accessibility_wide,
                input_srtm_wide,
                input_countries_wide,
                input_soils_wide)

#merge all data frames in list
database_complete <-
  df_list %>% 
  reduce(full_join, by = colnames(input_srtm_wide)[1])

# eventually save processed results
write_rds(database_complete,
          "../../datalake/mapme.protectedareas/output/matching/matching_frames/full_database.rds")

# ---- (4) create year-specific matching frames and save results---- 
## move files if they had been already created
newdirname="../../datalake/mapme.protectedareas/output/matching/matching_frames/arquived_2022-04-08"
dir.create(newdirname)
oldfiles=list.files("../../datalake/mapme.protectedareas/output/matching/matching_frames/",full.names = T,pattern = "matching_frame_")
sapply(oldfiles,
       function(x){
         file.copy(x,newdirname)
         file.remove(x)})

## function to create year specific matching frames
# get control cells
controls<-
  read_csv("../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_within_treated_buff50km_long.csv")
# get spatial data
wdpa_LA<-
  st_read("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_valid_simplified.gpkg")

wdpa_LA <-
  wdpa_LA %>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") %>%
  filter(STATUS != "Proposed") %>%
  filter(GEOMETRY_TYPE != "POINT")

# make valid
wdpa_LA<-
  st_make_valid(wdpa_LA)

# subset for treated
wdpa_kfw_treated <-
  wdpa_LA %>%
  filter(!is.na(bmz_n_1))


# # load full grid
honeycomb_subeset<-
  read_sf("../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset.gpkg")
# create ids
honeycomb_subeset$poly_id<-1:nrow(honeycomb_subeset)

# note: LOOP Currently fails for years with no data. 

for (i in c(2004:2017,2019)) {
  ## filter complete database to include only polygons from the treatment cells in a specific year
  print(paste("Starting year",i))
  # define year
  my_year <- i
  
  # filter complete database to exclude all PAs and their buffer zones
  source("code/processing-scripts/03.2_db_creation_buffer.R")
  
  database_complete_subset<-
    database_complete %>%
    filter(!.assetid%in%tmp_no_controls)
  # creat treatment column
  database_complete_subset$treatment=0
  
  # get poly_ids
  tmp_uids <-
    matching.frame %>%
    filter(., matching_year == my_year) %>%
    pull(poly_id)
  
  database_complete_subset_2 <-
    database_complete %>%
    filter(.assetid %in% tmp_uids)
  
  # creat treatment column
  database_complete_subset_2$treatment = 1
  
  # bind both datasets
  tmp_database_complete <-
    rbind(database_complete_subset, database_complete_subset_2)
  
  write_rds(
    tmp_database_complete,
    paste(
      "../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_",
      my_year,
      ".rds",
      sep = ""
    )
  )
}


# ---- check Matching frames against original polygon data in R -----
# year to check
my_year<-2007

# load data
matching_new_check=
  read_rds(paste("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_",my_year,".rds",sep=""))
# table the number of treatment polygons in the matching frame
table(matching_new_check$treatment)


# # load treated areas data
# wdpa_LA<-
#   st_read("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_valid_simplified.gpkg")
# 
# # clean data
# wdpa_LA <-
#   wdpa_LA %>%
#   filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") %>%
#   filter(STATUS != "Proposed") %>%
#   filter(GEOMETRY_TYPE != "POINT")
# 
# # subset for treated
# wdpa_kfw_treated <-
#   wdpa_LA %>%
#   filter(!is.na(bmz_n_1))

# shape to long to add start date
wdpa_kfw_treated_long <- 
  wdpa_kfw_treated %>% 
  pivot_longer(.,
               cols = starts_with("bmz_n"),
               values_to = "bmz_nummer")

# filter out such that were not treated
wdpa_kfw_treated_long <-
  wdpa_kfw_treated_long %>% 
  filter(!is.na(bmz_nummer))

# match start year
wdpa_kfw_treated_long<- project.data.reduced %>% 
  select(bmz_nummer,first_year,last_year) %>% 
  merge(wdpa_kfw_treated_long,.,by="bmz_nummer")

# subset for the specified year
wdpa_kfw_treated_long <- 
  wdpa_kfw_treated_long %>% 
  filter(first_year==my_year)

# filter matching data
matching_new_check_treated<- matching_new_check %>% 
  filter(treatment==1)

# merge data
honeycomb_subeset_merge2<-
  merge(honeycomb_subeset,matching_new_check_treated,by.y=".assetid",by.x="poly_id",)

# show in map
mapview::mapView(wdpa_kfw_treated_long)+mapView(honeycomb_subeset_merge2,col.regions ="red")
# clean up memory 
gc()

# ---- export data for qgis check -----
## 
honeycomb_subeset_merge2<-
  merge(honeycomb_subeset,matching_new_check,by.y=".assetid",by.x="poly_id",)

# get centroids
honeycomb_subeset_merge2_centroids<-
  st_centroid(honeycomb_subeset_merge2)

# select only relevant vars for check
honeycomb_subeset_merge2_centroids<-
  honeycomb_subeset_merge2_centroids %>% 
  select(poly_id,treatment,geometry)


# show in map
#mapView(wdpa_kfw_treated_long)+mapView(honeycomb_subeset_merge2_centroids,zcol="treatment")
# clean up memory 
write_sf(honeycomb_subeset_merge2_centroids,
         paste("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_",my_year,".gpkg",sep=""))

gc()


