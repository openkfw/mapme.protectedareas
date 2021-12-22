library(sf)
library(mapview)

# new database

# read in gfw data and create a single df
gfw_data<-
  list.files("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08/gfw/",
             pattern = "csv",full.names = T)

gfw_data<-lapply(gfw_data,read.csv)

gfw_data<-do.call(rbind,gfw_data)

# ----- check data for consistency ----- 
# how many treatments and controls?
table(gfw_data$treatment)

# area
hist(gfw_data$area_2000)
hist(gfw_data$area_2020)
summary(gfw_data$area_2020)

# loss
hist(gfw_data$loss_2001)
hist(gfw_data$loss_2020)
summary(gfw_data$loss_2020)

length(unique(gfw_data$poly_id))==length(unique(gfw_data$poly_id))
# so far it seems to be all fine with this data

# ----- join spatial data and make a visual check -----

fishnet <-
  read_sf(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_all_update_Dec-07.gpkg"
  )

merged_fishnet<-
  merge(fishnet, gfw_data, "poly_id")

# 
mapview(merged_fishnet, zcol = "area_2020",
        alpha.regions=0.6,
        layer.name="Forest cover in 2020",
        at=c(1,500,1000,2000,3000,5000,max(merged_fishnet$area_2020)),
        hide = TRUE)

merged_fishnet_supported<-merged_fishnet%>%
  filter(treatment.x==1)

# nrow(merged_fishnet)

mapview(merged_fishnet, 
        zcol = "loss_2020",
        alpha.regions=0.6,
        layer.name="Loss in 2020",
        at=c(1,100,250,500,750,1000,max(merged_fishnet$area_2020)),
        hide = FALSE)+
  mapview(merged_fishnet_supported,
          color="red",
          alpha.regions=0.1,
          layer.name="Trated",
          hide = FALSE)

# export data 
write_sf(merged_fishnet,"../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08/gfw/gfw_complete.gpkg")

# # ----- MERGE WITH OTHER DATA -----
merged_fishnet <-
  read_sf(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08/gfw/gfw_complete.gpkg"
  )

acc_5k <-
  read_csv(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/accessibility_5k_110mio_min.csv"
  )

acc_20k <-
  read_csv(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/accessibility_20k_110mio_min.csv"
  )

terrain <-
  read_csv(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/terrain_ruggedness_index.csv"
  )

clay <-
  read_csv(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/clay_content_depth_10_cm.csv"
  )

teow <-
  read_csv(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/teow_ecoregions_biomes.csv"
  )

worldpop <-
  read_csv(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/worldpop_final.csv"
  )

merged_fishnet <-
  merged_fishnet %>%
  rename(id = id.x)

merged_fishnet<-merged_fishnet %>% 
  merge(., pivot_wider(acc_5k,id_cols = "id",values_fn = {sum}), "id") %>% 
  merge(., pivot_wider(acc_20k,id_cols = "id",values_fn = {sum}), "id") %>% 
  merge(., pivot_wider(terrain,id_cols = "id",values_fn = {sum}), "id") %>% 
  merge(., pivot_wider(clay,id_cols = "id",values_fn = {sum}), "id") %>% 
  merge(., pivot_wider(teow,id_cols = "id",values_fn = {sum}), "id") %>% 
  merge(., pivot_wider(worldpop,id_cols = "id",values_fn = {sum}), "id")

names(merged_fishnet)

# export data 
write_sf(merged_fishnet,"../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08/fishnet_complete.gpkg")


# ---- check GFW data grid ---- 
test<-read_sf("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/100km/fishnet_within_countries_100km.gpkg")
colnames(test)

gfw_100km <-
  read_csv(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/100km/fishnet_within_countries_100km_GFW_wide.csv"
  )

test<-test %>% 
  merge(.,gfw_100km)

test<-test %>% 
  mutate(loss_2001_2010 = rowSums(across(loss_2001:loss_2010)))

test<-test %>% 
  mutate(loss_2011_2020 = rowSums(across(loss_2011:loss_2020)))

summary(test$loss_2001_2010)
summary(test$loss_2001)


View(st_drop_geometry(test))
write_sf(test,"../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/100km/fishnet_within_countries_100km_gfw.gpkg")


# ---- check GFW data all PAs ---- 
test<-read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_valid.gpkg")
colnames(test)

gfw_100km <-
  read_csv(
    "../../datalake/mapme.protectedareas/output/polygon/global_forest_watch/zonal_statistics_allPAs_wide.csv"
  )

test<-test %>% 
  merge(.,gfw_100km)

test<-test %>% 
  mutate(loss_2001_2010 = rowSums(across(loss_2001:loss_2010)))

test<-test %>% 
  mutate(loss_2011_2020 = rowSums(across(loss_2011:loss_2020)))

summary(test$loss_2001_2010)
summary(test$loss_2001)


View(st_drop_geometry(test))
write_sf(test,"../../datalake/mapme.protectedareas/output/polygon/global_forest_watch/zonal_statistics_allPAs_wide.gpkg")

# seems all good so far. 

test<-read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")
library(mapview)


mapview(test, 
        zcol = "REP_AREA",
        alpha.regions=0.6,
        layer.name="Area",
        at=c(1,1000,5000,10000,20000,max(test$REP_AREA)),
        hide = FALSE)

get()


# ----- merge matching results with spatial data -----
fishnet <-
  read_sf(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_all_update_Dec-07.gpkg"
  )
# read in gfw data and create a single df
matched_data<-
  list.files("../../datalake/mapme.protectedareas/output/tabular/regression_input/",
             pattern = "matched",full.names = T)

matched_data<-matched_data[-12]

matched_data<-lapply(matched_data,read.csv)

matched_data<-do.call(rbind,matched_data)


matched_data<-read.csv("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_2015.csv")

matched_data<-read.csv("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_2010.csv")

matched_data<-read.csv("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_2007.csv")

matched_data<-read.csv("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_2006.csv")

matched_data_merged<-
  merge(fishnet, matched_data, "poly_id")


table(matched_data_merged$treat_ever>0)

matched_data_merged <-
  matched_data_merged %>%
  filter(year == 2015)

colnames(matched_data_merged)

mapview(matched_data_merged, 
        zcol = "treat_ever",
        alpha.regions=0.6,
        layer.name="Treatment",
        hide = FALSE)

colnames(matched_data_merged)
nrow(matched_data_merged)
str(matched_data_merged)
