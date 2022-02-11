# Script: preprocess GIZ wdpa data
# Author: Johannes Schielein

library("dplyr")
library("readxl")
library("sf")

raw_giz<-
  read_xlsx("../../datalake/mapme.protectedareas/input/wdpa_giz/ProtectedAreas-GIZ_Stichtag31.12.2019_v2022-02-11.xlsx",sheet = 2,
            col_types = c("text","text","text","text","text","date","date","text","text","numeric","text","text","text","text","text","text","text","text","text","text","text","text","text"))


View(raw_giz)
# notes for storing data in the future
# column: WDPA ID: Should only contain one ID per row. if there area multiple IDs suggestion would be to make two or more columns.
# column: WDPA ID: in cases there is no clarity in the ID, this should be put in an extra comment column to avoid e.g. entries like "201 (5002? 41014?)"
# column G: check dates, one seems to be wrong. 
# column J: Should only contain numeric information, no character values. If area is not reported should be left blank. 
# general note: It would be usefull to have the country codes of the Areas in ISO3 format just as in the WDPA data. This would facilitate automatic download
# 23 PAs do not contain a WDPA ID currently (253 do). 
# Only 218 areas could be identified in the current WDPA database by the given 253 ids. This should be quality checked. 

# ----- preprocess GIZ data to be clean -----
raw_giz <-
  raw_giz %>%
  rename(wdpa_id = `WDPA ID`)

# recode missings
raw_giz$wdpa_id<-ifelse(raw_giz$wdpa_id=="N/A",NA,raw_giz$wdpa_id)

table(is.na(raw_giz$wdpa_id))
# split wdpa id column
wdpa_split<-
  stringr::str_split(raw_giz$wdpa_id,"\\/ | \\& | \\( | \\s*",simplify = T) %>% 
  as.data.frame()

colnames(wdpa_split)<-c("wdpa_id_1","wdpa_id_2","wdpa_id_3")
wdpa_split$wdpa_id_2<-gsub(x = wdpa_split$wdpa_id_2,pattern = "&\r\n",replacement = "")
wdpa_split$wdpa_id_2<-gsub(x = wdpa_split$wdpa_id_2,pattern = "\\(",replacement = "")
wdpa_split$wdpa_id_2<-gsub(x = wdpa_split$wdpa_id_2,pattern = "\\?",replacement = "")
wdpa_split$wdpa_id_3<-gsub(x = wdpa_split$wdpa_id_3,pattern = "\\?)",replacement = "")

# append the columns to the original data and delte wdpa id column
raw_giz<-
  cbind(raw_giz,wdpa_split)

raw_giz$wdpa_id<-NULL

# transpose the data to long with wdpa_id, project start, project end, project number 
raw_giz_long <-
  raw_giz %>%
  pivot_longer(cols = starts_with("wdpa_id"))

# filter out blank wdpa id obs. 
raw_giz_long <-
  raw_giz_long %>%
  filter(value != "")

# rename columns 
raw_giz_long<-raw_giz_long %>% 
  rename(wdpa_identifier=name,wdpa_id=value)

# ----- download and unzip the full database -----
download.file("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Feb2022_Public_shp.zip",
              destfile = "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp.zip")


unzip("../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp.zip",
      exdir = "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp")


unzip("../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_0.zip",
      exdir = "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_0")
unzip("../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_1.zip",
      exdir = "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_1")
unzip("../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_2.zip",
      exdir = "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_2")

# ----- filter the data for GIZ treated areas -----
wdpa_raw <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_0/WDPA_Feb2022_Public_shp-polygons.shp" 
  )

wdpa_raw<-wdpa_raw %>% 
  filter(WDPAID%in%raw_giz_long$wdpa_id)

nrow(wdpa_raw)

wdpa_raw_1 <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_1/WDPA_Feb2022_Public_shp-polygons.shp" 
  )

wdpa_raw_1<-wdpa_raw_1 %>% 
  filter(WDPAID%in%raw_giz_long$wdpa_id)

nrow(wdpa_raw_1)

wdpa_raw_2 <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_2/WDPA_Feb2022_Public_shp-polygons.shp" 
  )

wdpa_raw_2<-wdpa_raw_2 %>% 
  filter(WDPAID%in%raw_giz_long$wdpa_id)

nrow(wdpa_raw_2)

# bind the data together
wdpa_giz<-rbind(wdpa_raw,wdpa_raw_1)
wdpa_giz<-rbind(wdpa_giz,wdpa_raw_2)

# clean the data
wdpa_giz<-st_make_valid(wdpa_giz)

# write out the data
st_write(wdpa_giz,"../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_supported_2019_wdpaV_Feb2022.gpkg")

# ----- get all areas from the original data for supported countries -----
country_codes_giz<-unique(wdpa_giz$ISO3)
# split the codes to get individual countries
country_codes_giz<-str_split(country_codes_giz,";",simplify = F)
country_codes_giz<-unlist(country_codes_giz)
country_codes_giz<-country_codes_giz[country_codes_giz!=""]

wdpa_raw <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_0/WDPA_Feb2022_Public_shp-polygons.shp" 
  )

wdpa_raw<-wdpa_raw %>% 
  filter(ISO3%in%country_codes_giz)

nrow(wdpa_raw)

wdpa_raw_1 <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_1/WDPA_Feb2022_Public_shp-polygons.shp" 
  )

wdpa_raw_1<-wdpa_raw_1 %>% 
  filter(ISO3%in%country_codes_giz)

nrow(wdpa_raw_1)

wdpa_raw_2 <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_2/WDPA_Feb2022_Public_shp-polygons.shp" 
  )

wdpa_raw_2<-wdpa_raw_2 %>% 
  filter(ISO3%in%country_codes_giz)

nrow(wdpa_raw_2)

# bind the data together
wdpa_giz_all<-rbind(wdpa_raw,wdpa_raw_1)
wdpa_giz_all<-rbind(wdpa_giz_all,wdpa_raw_2)

# clean the data
wdpa_giz_all<-st_make_valid(wdpa_giz_all)

# write out the data
st_write(wdpa_giz_all,"../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_all_2019_wdpaV_Feb2022.gpkg")


# ----- apply polygon simplifcation for plotting -----
library("rmapshaper")

# simplify the rest
wdpa_giz_all<-
  ms_simplify(wdpa_giz_all)

# fix geometries
wdpa_giz_all <-
  st_make_valid(wdpa_giz_all)


# simplify the rest
wdpa_giz<-
  ms_simplify(wdpa_giz)

# fix geometries
wdpa_giz <-
  st_make_valid(wdpa_giz)


# write out the data
st_write(wdpa_giz_all,"../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_all_2019_wdpaV_Feb2022_simplified.gpkg")

# write out the data
st_write(wdpa_giz,"../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_supported_2019_wdpaV_Feb2022_simplified.gpkg")


