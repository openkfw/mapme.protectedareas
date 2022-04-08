devtools::install_github("mapme-initiative/mapme.biodiversity", force = TRUE, ref = "indicators")
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
  index = sample(1:nrow(aoi), 1000, replace = FALSE)
  aoi = aoi[index, ]
}

aoi2 = init_portfolio(aoi, years = 2000:2020,
                      outdir = "./data",
                      tmpdir = "./tmp",
                      cores = ncores,
                      verbose = TRUE)
aoi2 = get_resources(aoi2, resources = c("nasagrace"))

aoi3 = split(aoi2, (seq(nrow(aoi2))-1) %/% 100000) 

dir.create("results/drought", showWarnings = FALSE)

start_time = Sys.time()

for(i in 1:length(aoi3)){
  filename = file.path("results/drought", paste0("honeycomb_drought_", i, ".rds"))
  if(file.exists(filename)) next
  tmp = aoi3[[i]]
  tmp = calc_indicators(tmp, 
                        indicators = c("drought_indicator"),
                        stats_drought = c("mean", "median", "sd"),
                        engine = "extract")
  saveRDS(tmp, filename)
  rm(tmp); gc()
  aoi3[[i]] = NA
}

end_time = Sys.time()
print(end_time - start_time)