library(tidyverse)
library(sf)

pame_raw<-read_csv("../datalake/mapme.protectedareas/input/pame/protectedplanet-pame-2020-DEC.csv")
wdpa_supported<-read_sf("../datalake/mapme.protectedareas/output/wdpa-kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs.gpkg")

table(wdpa_supported$WDPA_PID%in%pame_raw$wdpa_id)
View(pame_raw)

View(st_drop_geometry(wdpa_supported))
