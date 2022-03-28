## GFW Processing GIZ
# Author: Johannes Schielein
devtools::install_github("mapme-initiative/mapme.forest",ref = "terra")
library("mapme.forest")

# read in polygons of interest
aoi = st_read("../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_all_2019_wdpaV_Feb2022.gpkg")


# filter out areas that are not needed for comparison
aoi<-
  aoi %>% 
  filter(DESIG_ENG!="UNESCO-MAB Biosphere Reserve") 

# download GFW data for the area of interest
raster_files = downloadGFW(shape = aoi,
                           basename = "pkgTest",
                           #dataset = "GFC-2020-v1.8",
                           outdir = "../../datalake/mapme.protectedareas/processing/global_mangrove_watch/giz_2022-02-12/",
                           keepTmpFiles = T)

# links to data
treeCover = ".../../datalake/mapme.protectedareas/processing/global_mangrove_watch/giz_2022-02-12/pkgTest_treecover2000.tif"
lossYear = "../../datalake/mapme.protectedareas/processing/global_mangrove_watch/giz_2022-02-12/pkgTest_lossyear.tif"
co2Layer = "../../datalake/mapme.protectedareas/processing/global_mangrove_watch/giz_2022-02-12/pkgTest_co2_emission.tif"

grass = "/usr/lib/grass78"

library("pbmcapply")
roi_stats <- pbmclapply(1:nrow(ca), function(i) {
  statsGRASS(
    grass = grass,
    addon_base = "../../johannes/mapme.protectedareas/data-raw/addons/",
    areas = aoi[i, ],
    tree_cover = treeCover,
    tree_loss = lossYear,
    tree_co2 = co2Layer,
    idcol = "WDPAID",
    thresholdClump = 6,
    thresholdCover = 10,
    years = 2001:2020,
    saveRaster = F
  )
}, mc.cores = 6)
