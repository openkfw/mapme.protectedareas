# additional script to create the database for the matching process (matching frames)
# Goal: Reduce control cells to exclude year specific buffer zones

# author: Johannes Schielein 
# last modification: 2022-04-09

# call relevant libs. 
library("sf")
library("tidyverse")
library("mapview")
options(scipen =999) # disable sicentific notation


# load buffer data
within_buff_results_df<-
  read_csv("../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_within_treated_buff50km_long.csv")

# goal: Get indiviudal yearly buffer zones which are excluded from controls
# shape to long to add start date
wdpa_kfw_treated_long <- 
  wdpa_kfw_treated %>% 
  pivot_longer(.,
               cols = starts_with("bmz_n"),
               values_to = "bmz_nummer")

# filter out such that were not treated
wdpa_kfw_treated_long <-
  wdpa_kfw_treated_long %>% 
  filter(!is.na(bmz_nummer))

# match start year
wdpa_kfw_treated_long<- project.data.reduced %>% 
  select(bmz_nummer,first_year,last_year) %>% 
  merge(wdpa_kfw_treated_long,.,by="bmz_nummer")

# projet areas
source("code/area_proj.R")
wdpa_kfw_treated_long_projected<-
  st_transform(wdpa_kfw_treated_long,
               crs = area_proj(wdpa_kfw_treated_long))


# buffer with a given distance
bufferdistance<-50000 # 50km
wdpa_kfw_treated_long_projected_buf <- wdpa_kfw_treated_long_projected %>%
  st_buffer(., bufferdistance)

# mapview::mapView(wdpa_kfw_treated_long_projected_buf, zcol = "first_year", legend = TRUE)

# project back for intersection
wdpa_kfw_treated_long_projected_buf<-  
  wdpa_kfw_treated_long_projected_buf %>% 
  st_transform(wdpa_kfw_treated_long_projected_buf,crs = st_crs(honeycomb_subeset))

# subset for year and intersect with buffer_polygons
honeycomb_subeset_buffers<-
  honeycomb_subeset %>% 
  filter(poly_id%in%within_buff_results_df$poly_id)

# set year of interest
tmp_year<-my_year
# filter buffer areas
tmp_buff<-
  wdpa_kfw_treated_long_projected_buf %>% 
  filter(first_year==tmp_year)

# intersect
tmp_no_controls<-
  st_intersects(honeycomb_subeset_buffers,tmp_buff,sparse = T)

tmp_no_controls<-
  unlist(lapply(tmp_no_controls,is_empty))

tmp_within_buff_results_df<-
  within_buff_results_df

tmp_within_buff_results_df$tmp_no_controls<-
  tmp_no_controls

tmp_within_buff_results_df <- 
  tmp_within_buff_results_df %>% 
  filter(tmp_no_controls==F)

# add to the list such poly ids that are within protected areas
tmp_intersected<-
  read_csv("../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_intersect_wdpa_long.csv")

tmp_no_controls<-
  unique(
    c(tmp_within_buff_results_df$poly_id,
      tmp_intersected$poly_id)
  )


