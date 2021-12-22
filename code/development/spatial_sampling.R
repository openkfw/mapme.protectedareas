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
??ms_erase

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

# ----  Grids for spatially balanced sampling ---- 
library("mapview")
mapviewOptions(platform = "leaflet")
library("sf")
library("terra")
library("raster")
library("rmapshaper")
library("GADMTools")

# note: Grids have been created with QGIS since sf package is not efficient in creating larger fishnets. 

pas_supported<-read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")
fishnet_10km_supported<-read_sf("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_within_10km.gpkg")
fishnet_10km_nonpa<-read_sf("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_within_coutnries_disjoint_10km.gpkg")


fishnet_100km<-read_sf("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_within_countries_100km.gpkg")
fishnet_100km_GFW<-read_csv("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_within_countries_100km_GFW_wide.csv")
fishnet_100km_Access_Small<-read_csv("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_within_countries_100km_accessibility_5k_110mio.csv")
#fishnet_100km_Access_Small<-read_csv("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_within_countries_100km_accessibility_50k_100k.csv")

fishnet_100km<-fishnet_100km%>%
  merge(.,fishnet_100km_GFW,by="id")

fishnet_100km<-fishnet_100km%>%
  merge(.,fishnet_100km_Access_Small,by="id")

accessibility_data<-raster("../../datalake/mapme.protectedareas/input/accessibility_to_cities/2015/acc_50k_100k.tif")

# show data. 
# mapview(fishnet_100km, zcol = "area_2020", alpha.regions=0.6)+
#   mapview(fishnet_10km_supported,  col.regions = "blue")+
#   mapview(fishnet_10km_nonpa,  col.regions = "grey60")

## erease the inside PA areas from the countries polygon
# load country borders
adminborders<-
  gadm_sf_loadCountries(unique(pas_supported$PARENT_ISO), # get countries based on unique country codes from PAs table
                        basefile = "../../datalake/mapme.protectedareas/input/gadm/",
                        simplify = 0.02) # simplify the polygons, should not exceed 0.25 to guarantee contigous country boundaries. 
adminborders<-adminborders$sf
# reproject
adminborders<-st_transform(adminborders, st_crs(fishnet_100km))
# intersect and clean
# fishnet_100km_erase<-
#   st_intersection(fishnet_100km,adminborders)

# intersect and clean
fishnet_100km_erase<-fishnet_100km
  # st_intersection(fishnet_100km,adminborders)

## cut out protected areas 
# load data
pas_all_simplified<-
  read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/geoprocessing/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_fixed_filtered_buffered_dissolved_buffered_simplified.gpkg")
# erease
fishnet_100km_erase <-
  ms_erase(target = fishnet_100km_erase, # clip the fishnet 
           erase = pas_all_simplified) # with the buffered PAs



## create variable for distance to projects
# get centroids
fishnet_100km_erase_centroids<-
  st_centroid(fishnet_100km_erase)

# calc distances
poly_distances <-
  st_distance(fishnet_100km_erase_centroids,
              st_transform(pas_supported, st_crs(fishnet_100km_erase_centroids)),
              by_element = FALSE)
# get minimum distance for each polygon
fishnet_100km_erase_centroids$dist_pa<-
  apply(poly_distances, 1, FUN = min)

#reduce to relevant vars
fishnet_100km_erase_centroids <-
  fishnet_100km_erase_centroids %>%
  st_drop_geometry(.)%>%
  dplyr::select(id, dist_pa)
 

# merge
fishnet_100km_erase<-fishnet_100km_erase%>%
  merge(.,fishnet_100km_erase_centroids,by="id")

# inverse the distance vector
fishnet_100km_erase$dist_pa_reverse<--fishnet_100km_erase$dist_pa+max(fishnet_100km_erase$dist_pa)


# for spatially weighted sampling see: https://cran.r-project.org/web/packages/spsurvey/vignettes/
library(spsurvey)
samplesize<-2000
## For exploration and visualization purposes first only with n = 100
sample_simple <- grts(fishnet_100km_erase,
                      n_base = samplesize)

# weigthed sampling Forest Cover
# note: for aux variable all obs must be larger then zero. 
fishnet_100km_erase$area_2010_aux<-fishnet_100km_erase$area_2010*5+1
sample_weigths_FC <- grts(fishnet_100km_erase, 
                          n_base = samplesize,
                          aux_var = "area_2010_aux")

# weigthed sampling Distances
fishnet_100km_erase$dist_pa_reverse<-fishnet_100km_erase$dist_pa_reverse+1
sample_weigths_Access <- grts(fishnet_100km_erase, 
                          n_base = samplesize,
                          aux_var = "dist_pa_reverse")

# combined variable
fishnet_100km_erase$sampling_combined<-
  fishnet_100km_erase$dist_pa_reverse*fishnet_100km_erase$area_2010*3+1

sample_weigths_combined <- grts(fishnet_100km_erase, 
                              n_base = samplesize,
                              aux_var = "sampling_combined")

# show data
mapview(sample_simple$sites_base,  
        col.regions = "red",
        layer.name="Sample (Random)")+
  mapview(sample_weigths_FC$sites_base,  
          col.regions = "blue",
          layer.name="Sample (weighted by forest cover = FC)",
          hide = TRUE)+
  mapview(sample_weigths_Access$sites_base,  
          col.regions = "yellow",
          layer.name="Sample (weighted by Accessibility = ACC)",
          hide = TRUE)+
  mapview(sample_weigths_combined$sites_base,  
          col.regions = "orange",
          layer.name="Sample (weighted by both = FC * ACC)",
          hide = TRUE)+
  mapview(pas_supported,  
          col.regions = "black",
          alpha.regions=1,
          layer.name="Supported PAs")+
  mapview(fishnet_100km_erase, zcol = "value", 
          alpha.regions=0.6,
          layer.name="Accessibility Small Towns in mins",
          at=c(1,500,1000,2000,3000,5000,max(fishnet_100km$area_2010)),
          hide = TRUE)+
  mapview(fishnet_100km_erase, zcol = "area_2020", 
          alpha.regions=0.6,
          layer.name="Forest cover in ha",
          hide = TRUE)+
  mapview(fishnet_100km_erase, zcol = "dist_pa", 
          alpha.regions=0.6,
          layer.name="Distance to Supported PA",
          hide = TRUE)

mapview(pas_supported,  
        zcol = "WDPAID",
        alpha.regions=0.6,
        layer.name="Supported PAs")

mapview(fishnet_10km_supported, 
        alpha.regions=0.6,
        col.regions = "black",
        layer.name="supported",
        hide = TRUE)+
  mapview(fishnet_10km_nonpa, 
          alpha.regions=0.6,
          col.regions = "red",
          layer.name="supported",
          hide = TRUE)
  




samplesize<-nrow(fishnet_10km_supported)*11
samplesize

sample_weigths_combined <- grts(fishnet_100km_erase, 
                                n_base = 2497,
                                aux_var = "sampling_combined")

samples_combined<-as.list(1:30) 

for(i in 1:30){
  tmp<- grts(fishnet_100km_erase,
          n_base = 2497,
          aux_var = "sampling_combined")
  samples_combined[[i]] <- tmp$sites_base
}

samples_combined2 <-
  do.call(rbind, samples_combined)

mapview(samples_combined2, 
        alpha.regions=0.6,
        layer.name="Distance to Supported PA",
        hide = TRUE)

# extract the cells that correspond to the sample
st_crs(fishnet_10km_nonpa)
st_crs(samples_combined2)

fishnet_all<-rbind(fishnet_10km_nonpa, fishnet_10km_supported)
nrow(fishnet_all)

mapview(fishnet_all, 
        alpha.regions=0.6,
        layer.name="Distance to Supported PA",
        hide = TRUE)

