wdpa_kfw %>% 
  filter(BM)


ids.melvin<-data.frame(ids=c(
1,555682914,
2,555682915,
3,900786,
4,900715,
5,478085,
6,555555655,
7,555555646,
8,555623649,
9,26621,
10,35271,
11,62052,
12,903037,
13,99652,
14,555629305,
15,555629306,
16,555629363,
17,555556016,
18,555629318,
19,555629331,
20,555629317,
21,555629313,
22,41027,
23,555582978,
24,555582979,
25,108073,
26,900668,
27,903016,
28,903013,
29,2234,
30,10754,
31,67744,
32,555600250,
33,81060,
34,555599979,
35,351721,
36,351720,
37,351931,
38,555600241,
39,478454,
40,555600297,
41,555599932,
42,555682146,
43,555600232,
44,555600202,
45,555576230,
46,555576325),dummy=rep(c(TRUE,FALSE),46))

ids.melvin <-
  ids.melvin %>%
  filter(dummy == FALSE)



wdpa_kfw<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

table(ids.melvin$ids%in%wdpa_kfw$WDPAID)


wdpa_kfw <-
  wdpa_kfw %>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") %>%
  filter(STATUS != "Proposed") %>%
  filter(GEOMETRY_TYPE != "POINT")

table(ids.melvin$ids%in%wdpa_kfw$WDPAID)



ids.melvin %>% 
  filter(ids%in%wdpa_kfw$WDPAID==FALSE)

ids.melvin.2 <-
  ids.melvin %>%
  filter(ids %in% wdpa_kfw$WDPAID == TRUE)

wdpa_kfw %>% 
  filter(WDPAID%in%ids.melvin.2$ids) %>% 
  filter(MARINE=="marine") %>% 
  pull(WDPAID)

wdpa_kfw %>% 
  filter(WDPAID%in%ids.melvin.2$ids) %>% 
  filter(MARINE!="marine") %>% 
  pull(bmz_n_1)

ids.melvin.2$ids%in%wdpa_kfw_long$WDPAID
ids.melvin.2$ids%in%grid_intersection$WDPAID


library(mapview)
wdpa_kfw %>% 
  filter(WDPAID%in%ids.melvin.2$ids) %>% 
  st_centroid() %>% 
  mapView(zcol="bmz_n_1")

mapView(wdpa_kfw,col.regions="bmz_n_1")




grid_intersection_sf <-
  read_sf(
    "../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_intersect.gpkg"
  )

# grid_intersection_sf <-
#   grid_intersection_sf %>%
#   filter(PA_intersect == 1)


grid_intersection_sf_centroids <-
  grid_intersection_sf %>%
  st_centroid(grid_intersection_sf)

# create new intersection
wdpa_kfw<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

wdpa_kfw_clean<-
  st_make_valid(wdpa_kfw)

wdpa_kfw_clean<-
  st_buffer(wdpa_kfw_clean,0.000001)

wdpa_kfw_clean<-st_transform(wdpa_kfw_clean,crs = st_crs(grid_intersection_sf_centroi))

point_in_PA <- st_join(grid_intersection_sf_centroi, wdpa_kfw_clean, join = st_within)

head(point_in_PA)
unique(point_in_PA$WDPAID)
table(is.na(point_in_PA$WDPAID))

ids.melvin.2$ids%in%
  point_in_PA$WDPAID

wdpa_kfw_clean_subset <-
  wdpa_kfw_clean %>%
  filter(WDPAID %in% ids.melvin.2$ids)

plotindex<-12

plot(
  st_geometry(wdpa_kfw_clean_subset_2[plotindex,]))

plot(grid_intersection_sf_centroids,add=T,col="blue")

mapView(wdpa_kfw_clean_subset_2[plotindex,])

wdpa_kfw_clean_subset_2 <-
  wdpa_kfw_clean_subset %>%
  filter(AREA_KM2>5)


wdpa_kfw_clean_subset_2$AREA_KM2
# 1 very small, no intersection with centroids 
# 2 marine
# 3 probably coast not in gadm
# 4 marine
# 5 located on a small island / marine
# 6. very small / no intersection
# 7. very small / no intersection
# 8 marine
# 9. small island / marine
# 10. very small / no intersection
# 11 small, close to coast
# 12 very small < 2sqkm
# 13 very small < 1sqkm
# 14 very small < 2sqkm
# 15 very small < 1sqkm
# 16 




