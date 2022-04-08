devtools::install_github("mapme-initiative/mapme.biodiversity", force = TRUE)
# devtools::install("../mapme.biodiversity/")
library(mapme.biodiversity)
library(sf)

# change the two values below accordingly
# ncores = round(parallel::detectCores() * 0.7 )
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
aoi2 = get_resources(aoi2, resources = "srtmdem")
aoi3 = split(aoi2, (seq(nrow(aoi2))-1) %/% 100000) 

dir.create("results/srtm", showWarnings = FALSE)

start_time = Sys.time()

for(i in 1:length(aoi3)){
  filename = file.path("results/srtm", paste0("honeycomb_tri_dem", i, ".rds"))
  if(file.exists(filename)) next
  tmp = aoi3[[i]]
  tmp = calc_indicators(tmp, indicators = c("tri", "elevation"),  
                        stats_tri = c("mean", "median", "sd"),
                        stats_elevation =  c("mean", "median", "sd"))

  saveRDS(tmp, filename)
  rm(tmp); gc()
  aoi3[[i]] = NA
}

end_time = Sys.time()
print(end_time - start_time)

rds_files = list.files("results/srtm/", pattern = ".rds$", full.names = TRUE)
dem_data = lapply(rds_files, function(f) readRDS(f))
dem_data = do.call(rbind, dem_data)
saveRDS(dem_data, "honeycomb_5sqkm_srtm.rds")