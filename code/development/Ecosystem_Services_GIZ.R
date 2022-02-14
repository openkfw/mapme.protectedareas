## Ecosystem_Services_GIZ
# author: Johannes Schielein
library(sf)
source("code/dopa-rest.R")

# read in polygons of interest
wdpa_giz_all = st_read("../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_all_2019_wdpaV_Feb2022.gpkg")

# ----- Redlist data ----- 
# get redlist data of the PAs
df_redlist_status<-
  lapply(wdpa_giz_all$WDPAID, 
         FUN = get_redlist_status)%>% 
  bind_rows()
