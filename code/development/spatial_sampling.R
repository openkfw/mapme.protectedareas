## Spatial Sampling
##  Protected areas
library("sf")
library("tidyverse")
library("GADMTools")
library("rmapshaper")
library("leaflet")
library("leaflet.extras") 
source("code/area_proj.R")

# read data
wdpa_kfw<-
  read_sf("data/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

# project areas
wdpa_kfw_proj<-st_transform(wdpa_kfw, 
                            area_proj(wdpa_kfw))

# delete all PAs that only have Point geometries in the WDPA database
wdpa_kfw_proj<-
  wdpa_kfw_proj%>%
  filter(GEOMETRY_TYPE!="POINT")

# filter for supported areas
wdpa_kfw_treatment_proj<-
  wdpa_kfw_proj%>%
  filter(bmz_n_1>0)

# load country borders
adminborders<-
  gadm_sf_loadCountries(unique(wdpa_kfw_treatment_proj$PARENT_ISO), # get countries based on unique country codes from PAs table
                        basefile = "../../datalake/mapme.protectedareas/input/gadm/",
                        simplify = 0.02) # simplify the polygons, should not exceed 0.25 to guarantee contigous country boundaries. 

adminborders<-adminborders$sf


# define area size for AOIs in sqkm and subsequently buffersize
aoi_areasize=5
buffersize<-sqrt(1/pi)*1000*aoi_areasize # the buffersize and radius is a function of geometric calculation of the area (rounded cirlce)

# create a 500 meter outward buffer
wdpa_kfw_treatment_proj_buff500_outward<-st_buffer(wdpa_kfw_treatment_proj,buffersize) # treatment areas
wdpa_kfw_proj_buff500_outward<-st_buffer(wdpa_kfw_proj,buffersize) # all PAs

# create a 500 meter inward buffer
wdpa_kfw_treatment_proj_buff500_inward<-st_buffer(wdpa_kfw_treatment_proj,-buffersize) # treatment areas
wdpa_kfw_proj_buff500_inward<-st_buffer(wdpa_kfw_proj,-buffersize) # all PAs


# see how many PAs do not have any geometry anymore, because they had been too small before and hence do not have area anymore with the inward buffer
table(as.vector(st_area(wdpa_kfw_treatment_proj_buff500_inward))==0)

# delte those from the dataset treatment
wdpa_kfw_treatment_proj_buff500_inward <-
  wdpa_kfw_treatment_proj_buff500_inward[-which(as.vector(st_area(wdpa_kfw_treatment_proj_buff500_inward)) ==
                                                  0), ]

# see how many PAs do not have any geometry anymore because they had been too small before and hence do not have area anymore with the inward buffer
table(as.vector(st_area(wdpa_kfw_proj_buff500_inward))==0)

# delte those from the dataset all PAs
wdpa_kfw_proj_buff500_inward <-
  wdpa_kfw_proj_buff500_inward[-which(as.vector(st_area(wdpa_kfw_proj_buff500_inward)) ==
                                        0), ]

# reproject the data again
wdpa_kfw_treatment_proj_buff500_inward<-
  st_transform(wdpa_kfw_treatment_proj_buff500_inward,crs = st_crs(wdpa_kfw))

# wdpa_kfw_nontreatment_proj_buff500_inward<-
#   st_transform(wdpa_kfw_nontreatment_proj_buff500_inward,crs = st_crs(wdpa_kfw))

# ---- 1. sample areas from supported PAs ----
# unionize polygons
wdpa_kfw_treatment_proj_buff500_inward_union <-
  st_union(wdpa_kfw_treatment_proj_buff500_inward)

## clip polygons to terrestrial areas with the admin boundary layer from GADM
wdpa_kfw_treatment_proj_buff500_inward_union_clip<-
  ms_clip(wdpa_kfw_treatment_proj_buff500_inward_union,adminborders)

# create a sample of areas within 
sample_treatment<-
  st_sample(wdpa_kfw_treatment_proj_buff500_inward_union_clip,
            size = 5000,
            type = "random",
            exact = FALSE, 
            by_polygon = TRUE)

# reproject again
sample_treatment<-st_transform(sample_treatment,st_crs(wdpa_kfw_treatment_proj))
# buffer the sample with 1 km buffer
sample_treatment_1km<-st_buffer(sample_treatment,buffersize)

sample_overlaps<-st_overlaps(sample_treatment_1km)
table(lapply(sample_overlaps,length)>0)
sample_overlaps<-which(lapply(sample_overlaps,length)>0)

# erase those from the sampled data
sample_treatment_1km<-sample_treatment_1km[-sample_overlaps,]
# compute the final length of sampled AOIs
length(sample_treatment_1km)


# ---- 2. sample areas from non-supported PAs ----
# subset PA buffered layer for non-supported PAs
wdpa_kfw_nontreatment_proj_buff500_inward<-
  wdpa_kfw_proj_buff500_inward%>%
  filter(is.na(bmz_n_1)==TRUE)

# unionize polygons
wdpa_kfw_nontreatment_proj_buff500_inward_union <-
  st_union(wdpa_kfw_nontreatment_proj_buff500_inward)

# clip polygons to terrestrial areas with the admin boundary layer from GADM
wdpa_kfw_nontreatment_proj_buff500_inward_union_clip<-
  ms_clip(wdpa_kfw_nontreatment_proj_buff500_inward_union,adminborders)

# erease from dataset such areas that overlap with supported areas
wdpa_kfw_nontreatment_proj_buff500_inward_union_clip_erease <-
  ms_erase(target = wdpa_kfw_nontreatment_proj_buff500_inward_union_clip, # erease from the nonsupported areas layer
          erase = st_as_sf(wdpa_kfw_treatment_proj)) # such areas that overlap with supported areas

# create a sample of areas within 
sample_nontreatment<-
  st_sample(wdpa_kfw_nontreatment_proj_buff500_inward_union_clip_erease,
            size = 5000,
            type = "random",
            exact = FALSE, 
            by_polygon = TRUE)

# buffer the sample with 1 km buffer
sample_nontreatment_1km<-
  st_buffer(sample_nontreatment,buffersize)

sample_nontreatment_overlaps<-
  st_overlaps(sample_nontreatment_1km)
table(lapply(sample_nontreatment_overlaps,length)>0)
sample_nontreatment_overlaps<-
  which(lapply(sample_nontreatment_overlaps,length)>0)

# erase those from the sampled data
sample_nontreatment_1km<-
  sample_nontreatment_1km[-sample_nontreatment_overlaps,]

# compute the final length of sampled AOIs
length(sample_nontreatment_1km)

# ----- 3. sample areas outside of protected areas -----
# unionize layer
wdpa_kfw_proj_buff500_outward_union<-
  st_union(wdpa_kfw_proj_buff500_outward)

# bring to same crs with adminborders. 
wdpa_kfw_proj_buff500_outward_union<-
  st_transform(wdpa_kfw_proj_buff500_outward_union,crs = st_crs(adminborders))

wdpa_kfw_proj_buff500_outward_union<-
  st_as_sf(wdpa_kfw_proj_buff500_outward_union)

# erease the inside PA areas from the countries polygon
adminborders_erase <-
  ms_erase(target = adminborders, # clip the admin boarders
           erase = st_as_sf(wdpa_kfw_proj_buff500_outward_union)) # with the buffered PAs


# unionize layer
adminborders_erase<-
  st_union(adminborders_erase)

# plot to check results
# plot(st_geometry(adminborders_erase))
# plot(st_geometry(wdpa_kfw_proj_buff500_outward_union),add=TRUE,col=alpha("blue", 0.4))

# create a sample of areas within 
sample_nonPA<-
  st_sample(adminborders_erase,
            size = 5000,
            type = "random",
            exact = FALSE, 
            by_polygon = TRUE)

#project layer
sample_nonPA<-st_transform(sample_nonPA, crs = st_crs(sample_nontreatment_1km))

# buffer the sample with 1 km buffer
sample_nonPA_1km<-st_buffer(sample_nonPA,buffersize)
sample_nonPA_overlaps<-st_overlaps(sample_nonPA_1km)
table(lapply(sample_nonPA_overlaps,length)>0)
sample_nonPA_overlaps<-which(lapply(sample_nonPA_overlaps,length)>0)

# erase those from the sampled data
sample_nonPA_1km<-sample_nonPA_1km[-sample_nonPA_overlaps,]
# compute the final length of sampled AOIs
length(sample_nonPA_1km)



# ----- save workspace -----
save.image("../../datalake/mapme.protectedareas/processing/spatial_sampling.Rdata")
load("../../datalake/mapme.protectedareas/processing/spatial_sampling.Rdata")

# ----- plot results -----
plot(st_geometry(sample_nontreatment_1km))
plot(st_geometry(sample_nontreatment_1km),add=TRUE,color="blue")
plot(st_geometry(sample_nonPA_1km),add=TRUE,color="red")
plot(st_geometry(sample_treatment_1km))

# project layers
#project layer
sample_nonPA_1km<-st_transform(sample_nonPA_1km, crs = st_crs(wdpa_kfw))
sample_nontreatment_1km<-st_transform(sample_nontreatment_1km, crs = st_crs(wdpa_kfw))
wdpa_kfw_treatment_proj<-st_transform(wdpa_kfw_treatment_proj, crs = st_crs(wdpa_kfw))
sample_treatment_1km<-st_transform(sample_treatment_1km, crs = st_crs(wdpa_kfw))
wdpa_kfw_nontreatment_proj_buff500_inward<-st_transform(wdpa_kfw_nontreatment_proj_buff500_inward, crs = st_crs(wdpa_kfw))
my_map <-
  leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$Esri.WorldShadedRelief, group="Topography") %>%
  addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group="Nightlights") %>%
  #addPolygons(data = wdpa_kfw_nontreatment_proj_buff500_inward, opacity = 0.3,color = "blue", group = "Other PAs (Inside Buffer)")%>%
  addPolygons(data = sample_nontreatment_1km,opacity = 0.9,color = "blue", group = "Sample other PAs")%>%
  addPolygons(data = sample_nonPA_1km,opacity = 0.9,color = "red", group = "Sample not PA")%>%
  addPolygons(data = sample_treatment_1km,opacity = 0.9,color = "yellow", group = "Sample supported PAs")%>%
  addPolygons(data = wdpa_kfw_treatment_proj,opacity = 0.3,color = "yellow",group = "KfW supported PAs (Inside Buffer)")%>%
  # addPolygons(data = adminborders,opacity = 0.9,color = "black", group = "Admin")%>%
  addLayersControl(
    baseGroups = c("OpenStreetMap","Satellite","Topography","Nightlights"),
    overlayGroups = c("Sample Supported PAs","Sample other PAs","Sample not PA","KfW Supported PAs"),#"Other PAs"
    options = layersControlOptions(collapsed = FALSE))%>%
  addFullscreenControl()
my_map

# ---- export data -----
# merge sample
samplecomplete <-
  rbind(
    cbind(st_as_sf(sample_nontreatment_1km), strata = "nonsupported"),
    cbind(st_as_sf(sample_nonPA_1km), strata = "nonPA"),
    cbind(st_as_sf(sample_treatment_1km), strata = "supported")
  )

table(samplecomplete$strata)

write_sf()