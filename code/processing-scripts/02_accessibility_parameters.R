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
aoi2 = get_resources(aoi2, resources = c("traveltime"),
                     range_traveltime = c(
                       "5k_10k", "10k_20k", "20k_50k", 
                       "50k_100k", "100k_200k", "200k_500k",
                       "500k_1mio", "1mio_5mio", "50k_50mio", 
                       "5k_110mio", "20k_110mio")
)

aoi3 = split(aoi2, (seq(nrow(aoi2))-1) %/% 100000) 

dir.create("results/accessibility", showWarnings = FALSE)

start_time = Sys.time()

for(i in 1:length(aoi3)){
  filename = file.path("results/accessibility", paste0("honeycomb_accessibility_", i, ".rds"))
  if(file.exists(filename)) next
  tmp = aoi3[[i]]
  tmp = calc_indicators(tmp, 
                        indicators = c("accessibility"),
                        stats_accessibility = c("mean", "median", "sd"),
                        engine = "extract")
  saveRDS(tmp, filename)
  rm(tmp); gc()
  aoi3[[i]] = NA
}

end_time = Sys.time()
print(end_time - start_time)

rds_files = list.files("results/accessibility/", pattern = ".rds$", full.names = TRUE)
data = lapply(rds_files, function(f) readRDS(f))
data = st_as_sf(purrr::map_dfr(data, rbind))
saveRDS(data, "honeycomb_5sqkm_accessibility.rds")
