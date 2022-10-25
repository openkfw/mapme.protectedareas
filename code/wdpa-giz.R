# Script: preprocess GIZ wdpa data
# Author: Johannes Schielein

library("dplyr")
library("readxl")
library("sf")

raw_giz<-
  read_xlsx("../../datalake/mapme.protectedareas/input/wdpa_giz/ProtectedAreas-GIZ_Stichtag31.12.2019_v2022-02-11.xlsx",sheet = 2,
            col_types = c("text","text","text","text","text","date","date","text","text","numeric","text","text","text","text","text","text","text","text","text","text","text","text","text"))

raw_giz<-
  read_xlsx("../../datalake/mapme.protectedareas/input/wdpa_giz/ProtectedAreas_GIZ-31.12.2019__2022-03-30.xlsx",sheet = 2,
            col_types = c("text","text","text","text","text","text","date","date","text","text","numeric","text","text","text","text","text","text","text","text","text","text","text","text","text","numeric","numeric","numeric"))


# ----- preprocess GIZ data to be clean -----
raw_giz <-
  raw_giz %>%
  rename(wdpa_id = `WDPA ID 1`)

# recode missings
raw_giz$wdpa_id<-ifelse(raw_giz$wdpa_id=="N/A",NA,raw_giz$wdpa_id)

table(is.na(raw_giz$wdpa_id))

# delete duplicate entries (same wdpa id and same project number)
colnames(raw_giz)
raw_giz<-
  raw_giz [!duplicated(raw_giz[c("wdpa_id","Project no. (PN)")]),]

# this removes 10 areas


# ----- download and unzip the full database -----
# download.file("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Feb2022_Public_shp.zip",
#               destfile = "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp.zip")
# 
# 
# unzip("../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp.zip",
#       exdir = "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp")
# 
# 
# unzip("../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_0.zip",
#       exdir = "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_0")
# unzip("../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_1.zip",
#       exdir = "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_1")
# unzip("../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_2.zip",
#       exdir = "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_2")

# ----- filter the data for GIZ treated areas -----
wdpa_raw <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_0/WDPA_Feb2022_Public_shp-polygons.shp" 
  )

wdpa_raw<-wdpa_raw %>% 
  filter(WDPAID%in%raw_giz$wdpa_id)

nrow(wdpa_raw) 

wdpa_raw_1 <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_1/WDPA_Feb2022_Public_shp-polygons.shp" 
  )

wdpa_raw_1<-wdpa_raw_1 %>% 
  filter(WDPAID%in%raw_giz$wdpa_id)

nrow(wdpa_raw_1)

wdpa_raw_2 <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_giz/WDPA_Feb2022_Public_shp/WDPA_Feb2022_Public_shp_2/WDPA_Feb2022_Public_shp-polygons.shp" 
  )

wdpa_raw_2<-wdpa_raw_2 %>% 
  filter(WDPAID%in%raw_giz$wdpa_id)

nrow(wdpa_raw_2)

# bind the data together
wdpa_giz<-rbind(wdpa_raw,wdpa_raw_1)
wdpa_giz<-rbind(wdpa_giz,wdpa_raw_2)

nrow(wdpa_giz) # only 215 out of 221 areas are found in the database

# clean the data
wdpa_giz<-st_make_valid(wdpa_giz)

# merge with GIZ data
wdpa_giz<-merge(wdpa_giz, raw_giz,by.x="WDPAID",
                by.y="wdpa_id")

colnames(wdpa_giz)
# reduce to relevant vars
wdpa_giz <-
  wdpa_giz %>%
  select("WDPAID",
         "ISO3",
         "Project no. (PN)",
         "Project name",
         "Project start",
         "Project end",
         "geometry")

colnames(wdpa_giz)<-c("WDPAID","ISO3","project_number","project_name","project_start","project_end","geometry")


wdpa_giz$project_number<-gsub("\\.","",wdpa_giz$project_number)

View(st_drop_geometry(wdpa_giz))
# write out the data
st_write(
  wdpa_giz,"../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_supported_31-12-2019_2022-07-15.gpkg",
  delete_layer = T)

write_csv(
  st_drop_geometry(wdpa_giz),
  "../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_supported_31-12-2019_2022-07-15.csv"
)

colnames(st_drop_geometry(wdpa_giz))


# ----- get all areas from the original data for supported countries -----
country_codes_giz<-unique(wdpa_giz$ISO3)
# split the codes to get individual countries
country_codes_giz<-stringr::str_split(country_codes_giz,";",simplify = F)
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
st_write(wdpa_giz_all,"../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_all_2019_wdpaV_Feb2022_V2.gpkg")



# ----- format KFW data in similar way. -----
wdpa_kfw<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

# reduce to relevant cols
wdpa_kfw <-
  wdpa_kfw %>%
  select(c("WDPAID", "ISO3", starts_with("bmz")))

# transform to long
wdpa_kfw<-
  wdpa_kfw %>%  pivot_longer(cols = starts_with("bmz"),values_to = "project_number")

# delete missings
wdpa_kfw<-wdpa_kfw %>% 
  filter(!is.na(project_number))


# read in project data
raw_kfw<-
  read.csv("../../datalake/mapme.protectedareas/input/kfw_finance/Portfolio_Auszahlungen.csv")

# reduce raw data
raw_kfw <-
  raw_kfw %>%
  select(
    "Stufe",
    "BMZ.Nummer",
    "Vorhaben",
    "Datum.Vertrag",
    "Datum.Projektabschluss",
    "Zusage"
  )

raw_kfw_agg<-raw_kfw %>% 
  group_by(BMZ.Nummer) %>% 
  summarize(Zusage=sum(Zusage),
            Datum.Vertrag=min(as.Date(Datum.Vertrag)),
            Datum.Projektabschluss=min(as.Date(Datum.Projektabschluss)),
            Vorhaben=unique(Vorhaben))


# merge data
wdpa_kfw<-
  merge(wdpa_kfw,raw_kfw_agg,by.x="project_number",by.y="BMZ.Nummer")

# reduce to relevant vars
wdpa_kfw <-
  wdpa_kfw %>%
  select(
    "WDPAID",
    "ISO3",
    "project_number",
    "Vorhaben",
    "Datum.Vertrag",
    "Datum.Projektabschluss",
    "Zusage"
  )

colnames(wdpa_kfw)<-c("WDPAID","ISO3","project_number","project_name","project_start","project_end","finance_pledged","geometry")

# write out the data
st_write(
  wdpa_kfw,"../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_V2.gpkg",
  delete_layer = T)

View(st_drop_geometry(wdpa_kfw))

write_csv(
  st_drop_geometry(wdpa_kfw),
  "../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_V2.csv")

colnames(st_drop_geometry(wdpa_kfw))
View(st_drop_geometry(wdpa_giz))
# ----- ARQUIVE -----
# # ----- apply polygon simplifcation for plotting -----
# library("rmapshaper")
# 
# # simplify the rest
# wdpa_giz_all<-
#   ms_simplify(wdpa_giz_all)
# 
# # fix geometries
# wdpa_giz_all <-
#   st_make_valid(wdpa_giz_all)
# 
# 
# # simplify the rest
# wdpa_giz<-
#   ms_simplify(wdpa_giz)
# 
# # fix geometries
# wdpa_giz <-
#   st_make_valid(wdpa_giz)
# 
# 
# # write out the data
# st_write(wdpa_giz_all,"../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_all_2019_wdpaV_Feb2022_simplified.gpkg")
# 
# # write out the data
# st_write(wdpa_giz,"../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_supported_2019_wdpaV_Feb2022_simplified.gpkg")
# 
# st_write(wdpa_giz_all,"../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_all_2019_wdpaV_Feb2022_simplified.gpkg")
# 
# 
# 
# test<-sf::read_sf("../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_supported_2019_wdpaV_Feb2022.gpkg")
# nrow(test)



