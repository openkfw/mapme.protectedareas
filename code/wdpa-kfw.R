# Routine for downloading all polygons from countries where PAs are supported by KfW
# Author: Johannes Schielein
library("tidyverse")
# library("devtools")
# we will use the latest wdpar version
# install_github("https://github.com/prioritizr/wdpar")
library("wdpar")

# ----- load data and check for missings ----- 
## load kfw list 
wdpa_kfw_db<-
  read_csv("data/wdpa-kfw_latinamerica_2021-04-22.csv")

# ## load wdpa data (nedds to be done only once)
# wdpa_url<-"https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Apr2021_Public_csv.zip"
# # link gathered from: https://d1gam3xoknrgr2.cloudfront.net/
# 
# # download the current wdpa database as csv
# download.file(wdpa_url, 
#               "../../datalake/mapme.protectedareas/input/wdpa-kfw/WDPA_Mar2021_Public_csv.zip")
# 
# # extract file contents
# unzip("../../datalake/mapme.protectedareas/input/wdpa-kfw/WDPA_Mar2021_Public_csv.zip",
#       exdir = "../../datalake/mapme.protectedareas/input/wdpa-kfw/")
# 
# # list files
# list.files("../../datalake/mapme.protectedareas/input/wdpa-kfw/")
# 
# # delete old files
# file.remove("../../datalake/mapme.protectedareas/input/wdpa-kfw/WDPA_Mar2021_Public_csv.zip")

# load database
wdpa_db<-
  read_csv("../../datalake/mapme.protectedareas/input/wdpa-kfw/WDPA_Apr2021_Public_csv.csv")

## NOTES REGARDING THE original KfW database
  # Different issues arise with the original data
  # (Problem 1) There was a mixture of WDPA_ID and WDPA_PIDs by the creator of the data. this was fixed below
  # (Problem 2) Some WDPA_IDs are invalid (fixed below)
  # (Problem 3) Because this database is based on two different rounds of data gathering
      # there are entries where WDPA_ID is duplicated, hence bmz_n_1 might also be duplicated and should be corrected. (fixed outside in excel)

## Note the matching should be done on PIDs which is the most detailed ID 
# test KfW data
table(wdpa_kfw_db$wdpa_pid%in%wdpa_db$WDPA_PID)

# there are currently  entries which are not in the WDPA database. Show those entries
wdpa_kfw_db_missings <-
  wdpa_kfw_db %>%
  filter(!wdpa_kfw_db$wdpa_pid %in% wdpa_db$WDPA_PID)

# View(wdpa_kfw_db_missings)

## --- fix wrong and missing WDPA IDs -----
# Los Cobanos
wdpa_kfw_db <-
  wdpa_kfw_db %>%
  mutate(wdpa_pid = replace(wdpa_pid, wdpa_pid == "107424", "555703444"))

# Sierra del Abra Tanchipa.
# Note: 101416_B is in spatial data but not this csv list from WDPA
# wdpa_kfw_db <-
#   wdpa_kfw_db %>%
#   mutate(wdpa_pid = replace(wdpa_pid, wdpa_pid == "101416_B", "101416"))

# Shell Beach PA. 
  # Note: 41057_A is in spatial data but not this csv list from WDPA
# wdpa_kfw_db <-
#   wdpa_kfw_db %>%
#   mutate(wdpa_pid = replace(wdpa_pid, wdpa_pid == "41057_A", "41057"))

# Sistema de Islas, Islotes y Puntas Guaneras
wdpa_kfw_db <-
  wdpa_kfw_db %>%
  mutate(wdpa_pid = replace(wdpa_pid, wdpa_pid == "55544090", "555544090"))

# Reserva Ecologica Estadual Da Juatinga IS PART OF Área De Proteção Ambiental De Cairuçu
wdpa_kfw_db <-
  wdpa_kfw_db %>%
  mutate(wdpa_pid = replace(wdpa_pid, name_kfw == "Juatinga", "19458"))


# ----- check remaining errors and describe raw databse----- 
# check for duplicates
wdpa_kfw_db %>%
  filter(duplicated(.[["wdpa_pid"]]))

# check for missings
wdpa_kfw_db %>%
  filter(!wdpa_kfw_db$wdpa_pid %in% wdpa_db$WDPA_PID)


# ----- merge data to get countries labels-----
wdpa_db$WDPA_PID<-as.character(wdpa_db$WDPA_PID)
wdpa_join_db<-
  full_join(wdpa_db,wdpa_kfw_db,by=c("WDPA_PID"="wdpa_pid"))

# get countries where kfw is active
wdpa_kfw_countries <-
  wdpa_join_db %>%
  filter(!is.na(bmz_n_1)) %>%
  with(unique(ISO3))

# ----- download and preprocess the polygon data -----

###  create function to download and preprocess with wdpar package
## NOTE
  # This function simplifies the geometries of the polygons
  # Its parameters can be adapted to be either more precise (which reduces processing speed later)
  # or to be more simple (which increases processing speed but leads to lower spatial accuracy)
  # the default parameters given here produce reasonable results especially for larger area assessments. 
  # for single or smaller area assessments it might be beneficial, however, to simplify with higher precision. 

## NOTE 2
  # There are two methods to get the polygon data. 
    # First is using API (provided by function). Second is downloading original data. 
    # For both datasets, the data is incomplete i.e. not all WDPA IDs from the KfW table are found. 
    # Therefore we combine both methods. 

wdpa_get_and_preprocess_withoverlaps <- function(my_iso=NULL,
                                                 my_snap_tolerance=5, # 5 meters per default
                                                 my_simplify_tolerance=5,
                                                 fetchdata=TRUE, # per default data is downloaded if not provide data
                                                 mydata=NULL) {
  out <- tryCatch(
    { message(paste("Starting to process ", my_iso))
      if (fetchdata == TRUE)
      {
        tmp.data <-
          wdpa_fetch(my_iso,
                     wait = TRUE,
                     download_dir =  tempdir())
      }else
        tmp.data <- mydata
      wdpa_clean(tmp.data,
                 erase_overlaps = F,
                 retain_status = NULL,
                 exclude_unesco = FALSE,
                 snap_tolerance = my_snap_tolerance,
                 simplify_tolerance = my_simplify_tolerance
      )
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
# -> 16 corresponds to the NA value, so no problem

# ----- check for empty geometries -----
wdpa_kfw_spatial%>%
  st_is_empty()%>%
  table()

which(st_is_empty(wdpa_kfw_spatial)==T)

# extract the wdpa ids of missings
wdpa_emptygeom_ids <-
  wdpa_kfw_spatial %>%
  filter(st_is_empty(.) == T)

# ----- clean data further -----
# check and remove  exact duplicates
wdpa_kfw_spatial <-
  wdpa_kfw_spatial %>%
  distinct()


# ----- join data and analyze output
# check if there are any data that could not be fetched
table(wdpa_kfw_db$wdpa_pid%in%wdpa_kfw_spatial$WDPA_PID)
# There are 8 PAs that do not have a correspondance in the spatial data. 
# filter them out 
missings_wdpa_pid<-
  wdpa_kfw_db%>%
  filter(!wdpa_kfw_db$wdpa_pid%in%wdpa_kfw_spatial$WDPA_PID)

View(missings_wdpa_pid)

## IMPORTANT NOTE: There are two PAs that cannot be found with a valid geometry via API (and download)
# those are 
  # Sierra del Abra Tanchipa (101416) and 
  # Sistema de Islas, Islotes y Puntas Guaneras (555544090)
  # Nevertheless they appear on the protectedplanet.net website. 
colnames(wdpa_kfw_db)
wdpa_kfw_db <- rename(wdpa_kfw_db,  WDPA_PID=wdpa_pid)

wdpa_kfw_spatial<-left_join(wdpa_kfw_spatial,wdpa_kfw_db)

# check the result
wdpa_kfw_spatial%>%
  filter(!is.na(bmz_n_1))

# 398 areas could be attributed

# ----- buffer results and save outputs  -----
# create a layer with buffer areas
wdpa_kfw_spatial_buffer5km<-st_buffer(wdpa_kfw_spatial,5000)
wdpa_kfw_spatial_buffer10km<-st_buffer(wdpa_kfw_spatial,10000)
wdpa_kfw_spatial_buffer15km<-st_buffer(wdpa_kfw_spatial,15000)

# project to WGS84
wdpa_kfw_spatial<-st_transform(wdpa_kfw_spatial,crs = 4326)
wdpa_kfw_spatial_buffer5km<-st_transform(wdpa_kfw_spatial_buffer5km,crs = 4326)
wdpa_kfw_spatial_buffer10km<-st_transform(wdpa_kfw_spatial_buffer10km,crs = 4326)
wdpa_kfw_spatial_buffer15km<-st_transform(wdpa_kfw_spatial_buffer15km,crs = 4326)

# write out
write_sf(wdpa_kfw_spatial,"data/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
write_sf(wdpa_kfw_spatial_buffer5km,"data/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_buffer5km.gpkg")
write_sf(wdpa_kfw_spatial_buffer10km,"data/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_buffer10km.gpkg")
write_sf(wdpa_kfw_spatial_buffer15km,"data/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_buffer15km.gpkg")

# ----- ARQUIVED: add polygons with empty geometries from original-----
## load spatial database
# wdpa_url<-"https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Apr2021_Public.zip"
# link gathered from: https://d1gam3xoknrgr2.cloudfront.net/

# download the current wdpa database as csv
# download.file(wdpa_url, 
#               "../../datalake/mapme.protectedareas/input/wdpa-kfw/wdpa_original_April2021/WDPA_Mar2021_Public.zip")

# extract file contents
# unzip("../../datalake/mapme.protectedareas/input/wdpa-kfw/wdpa_original_April2021/WDPA_Mar2021_Public.zip",
#       exdir = "../../datalake/mapme.protectedareas/input/wdpa-kfw/wdpa_original_April2021/WDPA_Apr2021_Public")


# delete old files
# file.remove("../../datalake/mapme.protectedareas/input/wdpa-kfw/wdpa_original_April2021/WDPA_Mar2021_Public.zip")

# # load spatial database
# wdpa_db_original<-
#   read_sf("../../datalake/mapme.protectedareas/input/wdpa-kfw/wdpa_original_April2021/WDPA_Apr2021_Public/WDPA_Apr2021_Public.gdb/")
# 
# # filter for the polygons without empty geometries
# wdpa_db_original %>%
#   filter(WDPA_PID %in% missings_wdpa_pid$wdpa_pid)
# 
# table(missings_wdpa_pid$wdpa_pid %in% wdpa_db_original$WDPA_PID)
# 
# colnames(wdpa_db_original)
# colnames(wdpa_kfw_spatial)
# 
# wdpa_db_original$WDPAID
# write_sf(wdpa_db_original,
#          "../../datalake/mapme.protectedareas/processing/wdpa_db_original_missings.gpkg")
# 
# # apply cleaning process
# wdpa_get_and_preprocess_withoverlaps(fetchdata = F,
#                                      mydata = wdpa_db_original)



# remove invalid geometry entries

# bind the cleaned polygons from the manually downloaded dataset