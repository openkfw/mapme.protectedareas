# Author: Johannes Schielein
# Purpose: combine drought and precipitation output data for further processing
# Date of last change: 2022-05-30

# call libs
library(sf)
library(tidyverse)

# precipitation indicator
# aggregate first file
files.first <-
  "../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/prec/honeycomb_prec_1.rds"

tmp_file<-
  readRDS(files.first)


tmp_file<-
  st_drop_geometry(tmp_file)

tmp_file<-
  unnest(tmp_file,cols = c(precipitation))

tmp_file_agg <-
  tmp_file %>%
  group_by(.assetid,years) %>% summarise(minprec_anomaly = min(anomaly),maxprec_anomaly = max(anomaly))

# create a list of files witout the first file
files <-
  list.files("../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/prec",full.names = T)

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
    unnest(tmp_file,cols = c(precipitation))
  
  tmp_file_agg_2 <-
    tmp_file %>%
    group_by(.assetid,years) %>% summarise(minprec_anomaly = min(anomaly),maxprec_anomaly = max(anomaly))
  
  tmp_file_agg<-
    rbind(tmp_file_agg,tmp_file_agg_2)
}

length(unique(tmp_file_agg$.assetid))

saveRDS(tmp_file_agg, 
        "../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/prec/honeycomb_prec_aggregated.rds")

hist(sample(tmp_file_agg$wetness_min,20000))

## plotting -only for test purposes
# test <-
#   tmp_file %>%
#   filter(.assetid %in%c(100000))



# test %>%
#   group_by(date = as.Date(cut(date, "month"))) %>% summarise(wetness_sum_monthly = sum(wetness_mean)/4) %>%
#   group_by(date = as.Date(cut(date, "year"))) %>% summarise(wetness_min = min(wetness_sum_monthly)) %>%
#   ggplot() +
#   geom_line(aes(date, wetness_min))


