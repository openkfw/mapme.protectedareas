# compare area estimates
library("sf")
library("mapview")
source("code/area_proj.R")
library("units")
pas_supported<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

# project area
area_proj(pas_supported)
pas_supported<-st_transform(pas_supported,crs=area_proj(pas_supported))
pas_supported$area_calc<-drop_units(st_area(pas_supported)/10^6)

pas_compare_area<-pas_supported%>%
  st_drop_geometry()%>%
  select(WDPAID,name_kfw,ISO3,area_sqkm_kfw,REP_AREA,area_calc)%>%
  rename(
    reported_kfw=area_sqkm_kfw,
    reported_wdpa=REP_AREA,
    polygon_area=area_calc,
    country_code=ISO3
  )%>%
  mutate_if(is.numeric, funs(round(.,digits=0)))

pas_compare_area$diff_kfw_TO_wdpa<-pas_compare_area$reported_kfw-pas_compare_area$reported_wdpa
pas_compare_area$diff_kfw_TO_wdpa_absolute<-sqrt(pas_compare_area$diff_kfw_TO_wdpa^2)
pas_compare_area$diff_wdpa_TO_polygon<-pas_compare_area$reported_wdpa-pas_compare_area$polygon_area


write_csv(pas_compare_area,"../../datalake/mapme.protectedareas/output/tabular/portfolio/area_compare.csv")


View(pas_compare_area)
sum(pas_compare_area$diff_kfw_TO_wdpa)
