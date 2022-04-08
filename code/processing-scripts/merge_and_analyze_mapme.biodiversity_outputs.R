library(sf)
library(dplyr)
library(tidyr)
library(magrittr)

# merged results
treeloss_merged = "honeycomb_5sqkm_treeloss.rds"
accessibility_merged = "honeycomb_5sqkm_accessibility.rds"
srtm_merged = "honeycomb_5sqkm_srtm.rds"
# drought_merged = "honeycomb_5sqkm_drough_indicator.rds"

# un-merged results
treeloss_rds = list.files("results/treeloss/", full.names = TRUE)
accessibility_rds = list.files("results/accessibility/", full.names = TRUE)
srtm_rds = list.files("results/srtm/", full.names = TRUE)
perc_rds = list.files("results/prec/", full.names = TRUE)
drought_rds = list.files("results/drought/", full.names = TRUE)
soil_rds = list.files("../../joerg/grid-portfolio/results/soilgrids/", full.names = TRUE)

# how-to merge into a single object
my_data<-soil_rds
my_name<-"soil"

data = lapply(my_data, function(f)
  readRDS(f))
data = st_as_sf(purrr::map_dfr(data, rbind))
saveRDS(
  data,
  paste(
    "../../datalake/mapme.protectedareas/output/polygon/grids/500m/data/",
    my_name,
    "_merged.rds",
    sep = ""
  )
)

# lets analyse a single file with 100,000 objects to learn how to work with the data
# reading in a dataset
srtm_vars = readRDS(srtm_rds[1])
head(srtm_vars)

# The indicators are nested list columns. In the example above 'tri' and elevation.
# We can access these to get a first impression about what we have just like a 
# normal list object
srtm_vars$tri[[1000]]
srtm_vars$elevation[[1000]]

# As we see here, both indicators just contain a single row per asset. We can thus
# savely unnest these columns without copying the geometry column which potentially
# can lead to RAM overflow. Below we will unnest just for a subset of the data
# in order to show how that works
srtm_vars[1:1000,] %>% unnest(c(tri, elevation))

# Most other indicators will have multiple rows per asset (i.e. because multiple years
# are analysed.) Unnesting these columns will lead to the geometry column to be copied
# substantially increasing the size of the resulting object. It is thus more secure
# to seperate geometry information from metadata and indicators during analaysis.
# We retain the variable '.assetid' in order to allow re-joining at a later stage.
(geometries = select(srtm_vars, .assetid))
(data = st_drop_geometry(srtm_vars))

# Now we can savely unnest the indicator columns and proceed with our forseen workflow.
# As a simple example, let's assume we were interested in the difference between the mean
# and median of the TRI and elevation in order to charachterize the skewness
# of the tri/elevation distribution within each gridcell. A standard tidyverse
# approach would look something like this:
(data %>% 
    unnest(c(tri, elevation)) %>% 
    select(.assetid, ends_with(c("mean", "median"))) %>%
    mutate(tri_diff = terrain_ruggedness_index_mean - terrain_ruggedness_index_median,
           dem_diff = elevation_mean - elevation_median) %>%
    select(.assetid, tri_diff, dem_diff) -> results)

# But we could also harness R's functional programming capabilites. For this,
# we will need to write a function that can be mapped over the listed columns.
# This way we actually omit unnesting these columns but we can still apply analysis
# functions to each asset. This is especially helpful if our aim is to return a single
# value per asset and indicator. This way, we can directly work on the original sf object
# without caring to much about copying the geometry columns. We'll use a little subset
# of the original dataset.
get_diff <- function(x) x[1] - x[2]
srtm_vars_test = srtm_vars[1:1000,]
srtm_vars_test$tri_diff = unlist(purrr::map(srtm_vars_test$tri, get_diff))
srtm_vars_test$dem_diff = unlist(purrr::map(srtm_vars_test$elevation, get_diff))
srtm_vars_test

# We now conducted a simple analysis based on our available indicators. Now, we might be
# interested to re-join our results with the geometry inforamtion, e.g. for visualization
# or data sharing. Below instructions will join our results with the geometries objects 
# based on the available .assetid
(results_w_geoms = left_join(geometries, results))
# In this case we obtained an sf object because of the left-join and the first argument
# being an sf object. If we reverse the order we would have to convert to sf because we obtain
# a data.frame
(left_join(results, geometries) %>%
    st_as_sf())
