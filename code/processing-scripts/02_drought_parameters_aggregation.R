# Author: Johannes Schielein
# Purpose: combine drought and precipitation output data for further processing
# Date of last change: 2022-05-30

# call libs
library(sf)
library(tidyverse)

# naca grace drought indicator
# aggregate first file
files.first <-
  "../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/drought/honeycomb_drought_1.rds"

tmp_file<-
  readRDS(files.first)


tmp_file<-
  st_drop_geometry(tmp_file)

tmp_file<-
  unnest(tmp_file,cols = c(drought_indicator))

tmp_file_agg <-
  tmp_file %>%
  group_by(.assetid,date = as.Date(cut(date, "month"))) %>% summarise(wetness_sum_monthly = sum(wetness_mean) / 4) %>%
  group_by(.assetid, date = as.Date(cut(date, "year"))) %>% summarise(wetness_min = min(wetness_sum_monthly))

# create a list of files witout the first file
files <-
  list.files("../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/drought",full.names = T)

files <-
  files[files!=files.first]

files <- sort(files)
# aggregate and rbind the rest
for (i in files[2:length(files)]){
  print(paste("starting to process",i))
  tmp_file<-
    readRDS(i)
  tmp_file<-
    st_drop_geometry(tmp_file)
  
  tmp_file<-
    unnest(tmp_file,cols = c(drought_indicator))
  
  tmp_file_agg_2 <-
    tmp_file %>%
    group_by(.assetid,date = as.Date(cut(date, "month"))) %>% summarise(wetness_sum_monthly = sum(wetness_mean) / 4) %>%
    group_by(.assetid, date = as.Date(cut(date, "year"))) %>% summarise(wetness_min = min(wetness_sum_monthly))
  tmp_file_agg<-
    rbind(tmp_file_agg,tmp_file_agg_2)
}

length(unique(tmp_file_agg$.assetid))

saveRDS(tmp_file_agg, 
        "../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/drought/honeycomb_drought_aggregated.rds")

hist(sample(tmp_file_agg$wetness_min,20000))

## Plotting only for test purposes
# test <-
#   tmp_file %>%
#   filter(.assetid %in%c(100000))

# goal: aggregate per month a rolling average over 4 weeks and extract minimum per year. 

# test %>%
#   group_by(date = as.Date(cut(date, "month"))) %>% summarise(wetness_sum_monthly = sum(wetness_mean)/4) %>%
#   group_by(date = as.Date(cut(date, "year"))) %>% summarise(wetness_min = min(wetness_sum_monthly)) %>%
#   ggplot() +
#   geom_line(aes(date, wetness_min))


