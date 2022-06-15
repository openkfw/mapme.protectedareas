devtools::install_github("mapme-initiative/mapme.biodiversity", force = TRUE)
# devtools::install("../mapme.biodiversity/")
library(mapme.biodiversity)
library(sf)

ncores = 12
sample = FALSE

dir.create("./data", showWarnings = FALSE)
dir.create("./tmp", showWarnings = FALSE)

if(!file.exists("aoi_wgs84.rds")){
  aoi = st_read("/datadrive/datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset.gpkg")
  aoi = st_transform(aoi, "EPSG:4326")
  saveRDS(aoi, "aoi_wgs84.rds")
} else {
  aoi = readRDS("aoi_wgs84.rds")
}

if(sample){
  set.seed(123)
  index = 1:1000
  aoi = aoi[index, ]
}

aoi2 = init_portfolio(aoi, years = 2000:2020,
                      outdir = "./data",
                      tmpdir = "./tmp",
                      cores = ncores,
                      verbose = TRUE)

aoi2 = get_resources(aoi2, resources = c("soilgrids"), 
                     layers = c("clay", "nitrogen", "phh2o", "soc"),
                     depths = c("0-5cm", "5-15cm", "30-60cm"),
                     stats = c("mean"))

aoi3 = split(aoi2, (seq(nrow(aoi2))-1) %/% 100000) 

dir.create("results/soilgrids", showWarnings = FALSE)

start_time = Sys.time()

for(i in 1:length(aoi3)){
  filename = file.path("results/soilgrids", paste0("honeycomb_soil_", i, ".rds"))
  if(file.exists(filename)) next
  tmp = aoi3[[i]]
  tmp = calc_indicators(tmp, 
                        indicators = c("soilproperties"),
                        stats_soil = c("mean", "median", "sd"),
                        engine = "extract")
  saveRDS(tmp, filename)
  rm(tmp); gc()
  aoi3[[i]] = NA
}

end_time = Sys.time()
print(end_time - start_time)


rds_files = list.files("results/soilgrids/", pattern = ".rds$", full.names = TRUE)
data = lapply(rds_files, function(f) readRDS(f))
data = st_as_sf(purrr::map_dfr(data, rbind))
saveRDS(data, "honeycomb_5sqkm_soilproperties.rds")