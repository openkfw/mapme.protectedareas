# script to create the database for the matching process (matching frames)
# Goal: Matching frames should be created on an annual base. Each matching frame should contain year-specific matching variables...
  # ... for treatment and controls cells (based on KfWs project documentation) 

# author: Johannes Schielein 
# last modification: 2022-004-01

# call relevant libs. 
library("sf")
library("tidyverse")
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

# ----- (4) Load matching variables, clean data, create additional variables and merge final results-----
## Forest area and emisssions
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
lossdata<-input_gfw_wide[3:22] - input_gfw_wide[(3:22) - 1]
colnames(lossdata)<-gsub("treecover","loss",colnames(lossdata))

# create lossdata for t-1 till t-3
input_gfw_wide <- cbind(input_gfw_wide, lossdata)

# create lossdata for t-3
lossdata_t3<- lossdata[(4:20) - 1] + lossdata[(4:20) - 2] +  lossdata[(4:20) - 3]
colnames(lossdata_t3)<-paste("loss_t3_",2004:2020,sep="")

input_gfw_wide <- cbind(input_gfw_wide, lossdata_t3)
colnames(input_gfw_wide)

rm(input_gfw)
save.image("../../datalake/mapme.protectedareas/output/matching/matching_frames/full_database_2022-03-31.Rdata")

## Accessibility
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

## Elevation and TRI 
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

## Soil characteristics
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


write_rds(database_complete,
          "../../datalake/mapme.protectedareas/output/matching/matching_frames/full_database.rds")

# ---- (4) create year-specific matching frames and save results---- 
## move files if they had been already created
newdirname="../../datalake/mapme.protectedareas/output/matching/matching_frames/arquived_2022-04-01"
dir.create(newdirname)
oldfiles=list.files("../../datalake/mapme.protectedareas/output/matching/matching_frames/",full.names = T,pattern = "matching_frame_")
sapply(oldfiles,
       function(x){
         file.copy(x,newdirname)
         file.remove(x)})

## function to create year specific matching frames
for (i in 2004:2020){
  print(paste("Starting to process year",i))
  # choose year
  my_year <- i
  ## 1. Subset the data to contain only matching observations from the respective year
  # get the unique ids of all AOIs that are not from the chosen year from the data matching.frame
  tmp_uids <-
    matching.frame %>%
    filter(., matching_year != my_year) %>%
    pull(poly_id)
  
  # remove those from the matching data set
  database_complete_tmp <-
    database_complete %>%
    filter(.,!.assetid %in% tmp_uids)
  
  # create column that indicates if the poly id is a treatment
  database_complete_tmp$treatment<-
    ifelse(database_complete_tmp$.assetid%in%matching.frame$poly_id,1,0)

  write_rds(
    database_complete_tmp,
    paste(
      "../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_",
      my_year,
      ".rds",
      sep = ""
    )
  )
}


# compare
# matching.new.2005=
#   read_rds("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_2005.rds")
# 
# table(matching.new.2005$treatment)
# colnames(matching.new.2005)
# 
# matching.new.2015=
#   read_rds("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_2015.rds")
# 
# table(matching.new.2015$treatment)




