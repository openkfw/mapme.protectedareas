# Routine for downloading all polygons from countries where PAs are supported by KfW
# Author: Johannes Schielein
library(tidyverse)

# load list 
wdpa_kfw_db<-
  read_csv("data/wdpa-kfw_latinamerica_2021-02-01.csv")

# download the current wdpa database as csv
download.file("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Mar2021_Public_csv.zip", 
              "/home/johannes/shared/datalake/WDPA_Mar2021_Public_csv.zip")

# link gathered from: https://d1gam3xoknrgr2.cloudfront.net/

# extract file contents
unzip("/home/johannes/shared/datalake/WDPA_Mar2021_Public_csv.zip",exdir = "/home/johannes/shared/datalake/mapma.protectedareas_wdpa")

# list files
list.files("/home/johannes/shared/datalake/mapma.protectedareas_wdpa")

# delete old files
file.remove("/home/johannes/shared/datalake/WDPA_Mar2021_Public_csv.zip")

# load database
wdpa_db<-
  read_csv("/home/johannes/shared/datalake/mapma.protectedareas_wdpa/WDPA_Mar2021_Public_csv.csv")

# test KfW data
table(wdpa_kfw_db$wdpa_pid%in%wdpa_db$WDPAID)
# there are currently six entries which are not in the WDPA database. Show those entries
wdpa_kfw_db_missings <-
  wdpa_kfw_db %>%
  filter(!wdpa_kfw_db$wdpa_pid %in% wdpa_db$WDPAID)

View(wdpa_kfw_db_missings)

# merge the data
wdpa_kfw_db$wdpa_pid<-as.double(wdpa_kfw_db$wdpa_pid)
wdpa_join_db<-
  full_join(wdpa_db,wdpa_kfw_db,by=c("WDPAID"="wdpa_pid"))

# get countries where kfw is active
wdpa_kfw_countries <-
  wdpa_join_db %>%
  filter(!is.na(bmz_n_1)) %>%
  with(unique(ISO3))

# ----- download and preprocess the polygon data -----
library("devtools")
# we will use the latest wdpar version
install_github("https://github.com/prioritizr/wdpar")
library("wdpar")

wdpa_get_and_preprocess_withoverlaps <-
  function(my_iso) {
    tmp.data <-
      wdpa_fetch(my_iso,
                 wait = TRUE,
                 download_dir =  tempdir())
      wdpa_clean(tmp.data,
                 erase_overlaps = F,
                 exclude_unesco = F)
  }



wdpa_get_and_preprocess_withoverlaps <- function(my_iso) {
  out <- tryCatch(
    { message(paste("Starting to process ", my_iso))
      tmp.data <-
        wdpa_fetch(my_iso,
                   wait = TRUE,
                   download_dir =  tempdir())
      wdpa_clean(tmp.data,
                 erase_overlaps = F,
                 exclude_unesco = F)
    },
    error=function(cond) {
      message(paste("PLEASE NOTE: There was a problem processing", my_iso, ".Try manually to download and merge the data!"))
      message("Here's the original error message:")
      message(cond)
      # return(NA)
    },
    finally={
      message(paste("Processed:", my_iso))
    }
  )    
  return(out)
}

# use the function on all countries and combine the results
wdpa_kfw_spatial<-
  lapply(wdpa_kfw_countries, 
         FUN = wdpa_get_and_preprocess_withoverlaps)%>% 
  bind_rows()

# check whether there is any country that was not processed
which(!wdpa_kfw_countries%in%wdpa_kfw_spatial$ISO3)
wdpa_kfw_countries[16]
# -> corresponds to the NA value, so no problem

# join kfw data
wdpa_kfw_spatial<-
  full_join(wdpa_kfw_spatial,wdpa_kfw_db,by=c("WDPAID"="wdpa_pid"))

# get countries where kfw is active
wdpa_kfw_spatial_onlysupported <-
  wdpa_kfw_spatial %>%
  filter(!is.na(bmz_n_1)) 

# save output data
write_sf(wdpa_kfw_spatial,"data/wdpa_kfw_spatial_latinamerica_2021-02-01_allPAs.gpkg")

colnames(wdpa_kfw_spatial)

plot(wdpa_kfw_spatial_onlysupported["WDPAID"])

write_sf(wdpa_kfw_spatial_onlysupported,"data/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs.gpkg")








