library("sf")
library("tidyverse")


# ----- (1b) Load data -----
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
unique(projectsbefore)

project.data.reduced<-
  project.data.reduced%>%
  filter(!bmz_nummer%in%projectsbefore)

# load original fishnet grid
fishnet_complete<-
  read_sf("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_all_update_Dec-07.gpkg")

# load GFW
GFW_wide<-
  read_sf("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08/gfw/gfw_complete.gpkg")

# load supported ara polygons
wdpa_kfw<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

sampling.ids<-
  fishnet_complete%>%
  filter(treatment==1)

sampling.ids<-
  st_centroid(sampling.ids)

sampling.ids<-st_transform(sampling.ids,crs=st_crs(wdpa_kfw))

# get intersection
sampling.interserctions<-
  st_intersects(sampling.ids,wdpa_kfw)

# see how many sampling points fall within more than one treated area
table(unlist(lapply(sampling.interserctions,function(x)length(x))))

# eliminate them from the sampling (and those that do not have an intersection)
intersection.elimination.candidates<-
  which(unlist(lapply(sampling.interserctions,function(x)length(x)))!=1)

# repeat the whole process
sampling.ids<-
  sampling.ids[-intersection.elimination.candidates,]

# get intersection
sampling.interserctions<-
  st_intersects(sampling.ids,wdpa_kfw)

# see how many sampling points fall within more than one treated area
table(unlist(lapply(sampling.interserctions,function(x)length(x))))
# if all are 1 then it is fine. 

# attach to the remaining the wdpa id
sampling.ids <-
  sampling.ids %>%
  add_column(wdpa_id = as.integer(unlist(st_drop_geometry(wdpa_kfw[unlist(sampling.interserctions), "WDPAID"]))))

# check results
wdpa_kfw%>%
  filter(WDPAID==31)%>%
  st_geometry%>%
  plot()

sampling.ids%>%
  filter(wdpa_id==31)%>%
  st_geometry%>%
  plot(.,add=T)


# ---- (2) Match Project Start and End Information to the sampling AOIs -----
# transform bmz table to long
wdpa_kfw_long <-
  wdpa_kfw %>%
  st_drop_geometry() %>%
  select(WDPAID, starts_with("bmz_n")) %>%
  pivot_longer(., cols = starts_with("bmz_n"), ) %>%
  filter(!is.na(value))

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
  sampling.ids.filter <-
    sampling.ids %>%
    filter(wdpa_id %in% matching.ids) %>%
    mutate(matching_year = matching.year)
  
  #  write into the AOI Frame the sampling year
  return(sampling.ids.filter)
}

# apply the function for all years
matching.frame<-do.call(rbind,lapply(2000:2020,f.matchingyear))

# ---- (3) Load data, merge, clean and create additional variables for matchting ----
# load again sampling ids
sampling.ids.original<-
  read_sf("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_all_update_Dec-07.gpkg")
# function to load matching data and transform to wide
f.load <- function(my_data_dir)
{ my_data<-read.csv(my_data_dir) %>% 
  distinct() %>% 
  pivot_wider(id_cols = "id")
}

# apply on all relevant datasets which are structured in long
matching_data <-
  lapply(
    c(
      "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/accessibility_5k_110mio_min.csv",
      "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/accessibility_20k_110mio_min.csv",
      "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/clay_content_depth_10_cm.csv",
      "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/teow_ecoregions_biomes.csv",
      "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/terrain_ruggedness_index.csv",
      "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/worldpop_final.csv"
    ),
    f.load
  )
# left join all of the data
matching_data_combined <-
  matching_data %>%
  reduce(left_join, by = "id")
# add treatment variable
matching_data_combined <-
  left_join(matching_data_combined,
            sampling.ids.original)

# remove irrelevant columns
matching_data_combined <-
  matching_data_combined %>%
  select(-c(
    #geom,
    # clay_content_10_cm,
    starts_with("teow_intersect"),
    # biome_intersect_sqkm_,
    terrain_ruggedness_index_median,
    terrain_ruggedness_index_standard_deviation
  ))

# rename columns
colnames(matching_data_combined)
matching_data_combined<-
  dplyr::rename(matching_data_combined,
                travel_time_to_nearby_cities_min_5k_100mio=travel_time_to_nearby_cities_min.x,
                travel_time_to_nearby_cities_min_20l_100mio=travel_time_to_nearby_cities_min.y)

## replace NAs in biomes data with zero
matching_data_combined <-
  matching_data_combined %>%
  mutate(across(starts_with("biome_"), ~ replace_na(.x, 0)))

# create function to get dominant biome
matching_data_combined %>%
  select(starts_with("biome_intersect")) %>%
  max.col(., ties.method = "random")

matching_data_combined$biome_max <-
  matching_data_combined %>%
  select(starts_with("biome_intersect")) %>%
  rownames_to_column() %>%
  gather(volumn, value, -rowname) %>%
  group_by(rowname) %>%
  filter(rank(-value, ties.method = "random") == 1) %>%
  pull(volumn)

# random selection if there are two maxima
matching_data_combined$biome_max <-
  gsub("biome_intersect_sqkm_",
       "",
       matching_data_combined$biome_max)

# remove other biome columns
matching_data_combined <-
  matching_data_combined %>%
  select(-starts_with("biome_intersect_sqkm_"))


## ADMIN AREAS 
countries<-list.files("../../datalake/mapme.protectedareas/input/GADM/shp//")
## First approach. we will only match on country level (gadm 0), not lower admin areas
countries<-
  countries[which(grepl("0.shp$",countries)==TRUE)] # level 0 for countrym, level 1 for states, etc. see GADM metadata


# load data
countries <-
  lapply(countries, function(x) {
    read_sf(paste(
      "../../datalake/mapme.protectedareas/input/GADM/shp/",
      x,
      sep = ""
    ))
  })

countries<-
  do.call(rbind,countries)

# get intersection (needs to be done on centroids to avoid boarder crossings)
country.intersections<-
  st_intersects(st_transform(st_centroid(st_as_sf(matching_data_combined)),st_crs(countries)),
                countries,sparse=T)

# subsitute null values with NAs in results
vector.missings<-which(unlist(lapply(country.intersections,length))==0)
country.intersections[vector.missings]<-NA

# add column to data
matching_data_combined$country<-unlist(country.intersections)

# recode the column 
a = matching_data_combined$country
a_levels = sort(unique(matching_data_combined$country))
a_labels = countries$NAME_0
matching_data_combined$country<-
  a_labels[ match(a,a_levels) ]

rm(a,a_labels,a_levels)

## add GFW data
colnames(matching_data_combined)

matching_data_combined<-
  left_join(matching_data_combined,st_drop_geometry(GFW_wide),"poly_id")

colnames(matching_data_combined)

## drop some unnecessary columns
matching_data_combined<-matching_data_combined %>% 
  select(.,-c(left:bottom)) %>% 
  select(.,-c(geom)) %>% 
  select(.,-c(id.x:treatment.y))

colnames(matching_data_combined)

# drop c


# ---- (4) create year-specific matching frames ---- 
for (i in 2003:2020){
  print(paste("Starting to process year",i))
  # choose year
  my_year <- i
  ## 1. Create speciic FOREST COVER AND FOREST COVER LOSS variables
  # calculate average forest cover loss over past three years before start
  matching_data_combined$average_fcl_matchingyear <-
    matching_data_combined %>%
    select(starts_with("loss") &
             ends_with(as.character(c(
               my_year, my_year - 1, my_year - 2
             )))) %>%
    rowSums(na.rm = T)
  
  # remove all other loss variables from the matching frame
  matching_data_combined_tmp <-
    matching_data_combined %>%
    select(-c(starts_with("loss")))%>%
    select(-c(starts_with("co2")))
  
  # remove all area columns but the one with fc from respective year
  matching_data_combined_tmp <-
    matching_data_combined_tmp %>%
    select(-c(starts_with("area") &
                !ends_with(as.character(my_year))))
  
  # rename the forest cover column
  matching_data_combined_tmp <-
    matching_data_combined_tmp %>%
    rename(fc_area_matchingyear = names(select(., starts_with("area"))))
  
  ## 2. Create specific POPULATION GROWTH variables
  # calculate average population growth
  tmp_pop <- matching_data_combined_tmp %>%
    select(.,
           starts_with("population") & ends_with(as.character(c(
             my_year, my_year - 2
           ))))
  colnames(tmp_pop) <- c("last", "first")
  
  matching_data_combined_tmp <-
    matching_data_combined_tmp %>%
    mutate(average_popgrowth = tmp_pop$last - tmp_pop$first)
  
  # delete all other popvars
  matching_data_combined_tmp <-
    matching_data_combined_tmp %>%
    select(-starts_with("population"))
  
  # delete tmpdata
  rm(tmp_pop)
  
  ## 3. Subset the data to contain only matching observations from the respective year
  # get the unique ids of all AOIs that are not from the chosen year from the data matching.frame
  tmp_uids <-
    matching.frame %>%
    filter(., matching_year != my_year) %>%
    pull(id)
  
  # remove those from the matching data set
  matching_data_combined_tmp <-
    matching_data_combined_tmp %>%
    filter(.,!id %in% tmp_uids)
  
  ## 4. Export the final matchingframes
  # delte geom_column
  matching_data_combined_tmp$geom <- NULL
  write_csv(
    matching_data_combined_tmp,
    paste(
      "../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_",
      my_year,
      ".csv",
      sep = ""
    )
  )
}

# ----- (5) Export data for the model -----
# colnames(matching_data_combined)
# model_data <-
#   matching_data_combined %>%
#   select(UID, starts_with("loss"))
# 
# write_csv(
#   model_data,
#   "../../datalake/mapme.protectedareas/output/matching/model_frames/fcl_supported_AND_nonPas.csv"
# )
# 
# # add financial data
# write_csv(
#   project.data.reduced,
#   "../../datalake/mapme.protectedareas/output/matching/model_frames/projectdata_supported.csv"
# )
# 
# write_csv(
#   wdpa_kfw_long,
#   "../../datalake/mapme.protectedareas/output/matching/model_frames/keys_wdpaid_bmz.csv"
# )
# add PA specific data

# add treatment type variables 

# export data







# ---- ARQUIVE -----
# 1) Get WDPA IDs for Sample data and delete sample AOIs that are within overlapping supported PAs
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
unique(projectsbefore)

project.data.reduced<-
  project.data.reduced%>%
  filter(!bmz_nummer%in%projectsbefore)

# find out how many PAs will be removed from the sample 
supportedPAs<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

# connect sampling points to strata
sampling.ids<-
  read_sf("../../Om/test/sampling_October-20-2021.gpkg")

sampling.ids<-
  st_centroid(sampling.ids)

wdpa_kfw<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

sampling.ids<-
  sampling.ids%>%
  filter(strata=="supported")

# get intersection
sampling.interserctions<-
  st_intersects(sampling.ids,wdpa_kfw)

# see how many sampling points fall within more than one treated area
table(unlist(lapply(sampling.interserctions,function(x)length(x))))

# eliminate them from the sampling (and those that do not have an intersection)
intersection.elimination.candidates<-
  which(unlist(lapply(sampling.interserctions,function(x)length(x)))!=1)

# repeat the whole process
sampling.ids<-
  sampling.ids[-intersection.elimination.candidates,]

# get intersection
sampling.interserctions<-
  st_intersects(sampling.ids,wdpa_kfw)

# see how many sampling points fall within more than one treated area
table(unlist(lapply(sampling.interserctions,function(x)length(x))))
# if all are 1 then it is fine. 

# attach to the remaining the wdpa id
sampling.ids <-
  sampling.ids %>%
  add_column(wdpa_id = as.integer(unlist(st_drop_geometry(wdpa_kfw[unlist(sampling.interserctions), "WDPAID"]))))

# check results
wdpa_kfw%>%
  filter(WDPAID==31)%>%
  st_geometry%>%
  plot()

sampling.ids%>%
  filter(wdpa_id==31)%>%
  st_geometry%>%
  plot(.,add=T)

# export results
# st_write(
#   sampling.ids,
#   dsn = paste("../../datalake/mapme.protectedareas/output/polygon/sampling/sampling_AOIs_",
#               format(Sys.time(), "%B-%d-%Y-%H:%M:%S"),
#               "_SUPPORTED_INDEXED.gpkg",
#               sep=""),
#   driver = "GPKG")
