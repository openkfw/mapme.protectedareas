library("cem")
# ----- get data -----
# load individual matching frames
   # note: those contain samples AOIs from 2002-2020 for supported PAs and non-protected areas. 

matching_db<-
  read_csv("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_2015.csv")

# ---- apply Coarse Exact Matching -----

test<-matching_data_combined%>%
  select(!starts_with("biome"))

imbalance(
  test$treatment,
  as.data.frame(test),
  drop = c("treatment","UID",""))

?imbalance
results_matching_PAs <-
  cem("treatment",
      as.data.frame(test),
      drop =  c("treatment","UID"),
      eval.imbalance = TRUE)

# check matched successes
results_matching_PAs$tab

# check imbalance
results_matching_PAs$imbalance

# check variable breaks
results_matching_PAs$breaks

# 
results_matching_PAs$group.idx



# treatment data & variables that should be dropped (cutoffs are generated automatically according to Mr. Melvin)

# outputs: matrix mat (output with matches for control and treatment)
# imbalance table after. 
# eventually "relaxed" to create several matches



