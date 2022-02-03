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
library(sf)
library(mapview)

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


# # ----- other data -----
# acc_5k<-
#   read_csv("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/accessibility_5k_110mio_min.csv")
# 
# acc_20k<-
#   read_csv("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/accessibility_20k_110mio_min.csv")
# 
# terrain<-
#   read_csv("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08_Om/terrain_ruggedness_index.csv")

# seems all good so far. 
# export data 
write_sf(merged_fishnet,"../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08/gfw/gfw_complete.gpkg")




