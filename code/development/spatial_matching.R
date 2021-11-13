library("cem")
library("ggplot2")

# ----- get data -----
# load individual matching frames
   # note: those contain samples AOIs from 2002-2020 for supported PAs and non-protected areas. 

# test for 2015
matching_db<-
  read_csv("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_2003.csv")

# ---- apply Coarse Exact Matching -----

imbalance(
  matching_db$treatment,
  as.data.frame(matching_db),
  drop = c("treatment","UID","biome_max","country"))

results_matching_PAs <-
  cem("treatment",
      as.data.frame(matching_db),
      drop =  c("treatment","UID","biome_max","country"),
      eval.imbalance = TRUE)

# check matched successes
results_matching_PAs$tab

# check imbalance
results_matching_PAs$imbalance

# check variable breaks
results_matching_PAs$breaks

# ----- adjust breaks -----
# 1. Travel time: travel_time_to_nearby_cities_min_5k_10k
ggplot(data = matching_db)+
  geom_histogram(aes(travel_time_to_nearby_cities_min_5k_10k))

results_matching_PAs$breaks$travel_time_to_nearby_cities_min_5k_10k
# suggeste breaks 0-120 min, 120-300 min, 300 - 1440, > 1440




