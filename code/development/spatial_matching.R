library("cem")
library("ggplot2")

# ----- get data -----
# load individual matching frames
   # note: those contain samples AOIs from 2002-2020 for supported PAs and non-protected areas. 

# test for 2015
matching_db<-
  read_csv("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_2015.csv")

# get all data
matching_db_list<-
  lapply(list.files("../../datalake/mapme.protectedareas/output/matching/matching_frames/"), function(x) {
    read_csv(paste(
      "../../datalake/mapme.protectedareas/output/matching/matching_frames/",
      x,
      sep = ""
    ))
  })


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


results_matching_PAs <-
  cem("treatment",
      as.data.frame(matching_db),
      drop =  c("treatment","UID","biome_max","country"),  keep.all=TRUE)

relax.cem(results_matching_PAs, as.data.frame(matching_db), depth=1)

# check matched successes
results_matching_PAs$tab

# check imbalance
results_matching_PAs$imbalance

# check variable breaks
results_matching_PAs$breaks

# ----- adjust breaks -----
## ---- 1) Travel time: travel_time_to_nearby_cities_min_5k_10k ----
ggplot(data = matching_db)+
  geom_histogram(aes(travel_time_to_nearby_cities_min_5k_10k,fill=as.factor(treatment)),alpha = 0.7, position="dodge")

ggplot(data = matching_db)+
  geom_histogram(aes(travel_time_to_nearby_cities_min_50k_100k,fill=as.factor(treatment)),alpha = 0.7, position="dodge")

# create list for manual cutoff points
cutoffs_list<-
  results_matching_PAs$breaks

# table default cutoffs CEM
table(cut(matching_db$travel_time_to_nearby_cities_min_50k_100k,
          results_matching_PAs$breaks$travel_time_to_nearby_cities_min_5k_10k),
      matching_db$treatment)

# define proposed cuttoffs
proposed_cutoffs_traveltime<-
  c(0,120,300,max(matching_db$travel_time_to_nearby_cities_min_5k_10k,na.rm = T)) # 0-2hrs, 2-6 hrs, >6 hrs

# show the distribution in the proposed cutoffs (small towns)
table(cut(matching_db$travel_time_to_nearby_cities_min_5k_10k,
          proposed_cutoffs_traveltime),
      matching_db$treatment)

# show the distribution in the proposed cutoffs (larger cities)
table(cut(matching_db$travel_time_to_nearby_cities_min_50k_100k,
          proposed_cutoffs_traveltime),
      matching_db$treatment)

# change list to proposed cutoffs
cutoffs_list$travel_time_to_nearby_cities_min_5k_10k<- 
  proposed_cutoffs_traveltime
cutoffs_list$travel_time_to_nearby_cities_min_50k_100k<-
  proposed_cutoffs_traveltime
 

## 2) ---- Clay Content ins soils ----
# create histogramm
ggplot(data = matching_db)+
  geom_histogram(aes(clay_content_30_cm,fill=as.factor(treatment)),alpha = 0.7, position="dodge")

# table default cutoffs CEM
table(cut(matching_db$clay_content_30_cm,
          results_matching_PAs$breaks$clay_content_30_cm),
      matching_db$treatment)

# -> no change because 
  # different clay contents are appropiate for different crops i.e. there is no threshold for suitable/not suitable for agriculture
  # cutoffs seem to be reasonable

## 3) ---- Terrain Ruggedness ----
# create histogramm
ggplot(data = matching_db)+
  geom_histogram(aes(terrain_ruggedness_index_mean,fill=as.factor(treatment)),alpha = 0.7, position="dodge")

table(cut(matching_db$terrain_ruggedness_index_mean,
          results_matching_PAs$breaks$terrain_ruggedness_index_mean),
      matching_db$treatment)

# definition of TRI see: https://community.esri.com/t5/water-resources-blog/terrain-ruggedness-index-tri-and-vector-ruggedness/ba-p/884340
# interpretation: 
  # We are dealing with 30 meter resolution in original DEM data. This means that a TRI of 10 is an average slope of 30%. A
  # According to some websites the maximum slope for a agricultural mechanization should be 15 degrees or 26%
  # We suggest the following cutoff points (0-5) for easy mechanization 5-10 for medium mechanization and >10 for no mechanization
  # note, that the TRI values are averaged. This means that we could have parts of the AOI with high TRIs and low TRIs which would result in medium TRI. 


# define proposed cuttoffs
proposed_cutoffs_tri<-
  c(0,5,10,max(matching_db$terrain_ruggedness_index_mean,na.rm = T))


# show the distribution in the proposed cutoffs 
table(cut(matching_db$terrain_ruggedness_index_mean,
          proposed_cutoffs_tri),
      matching_db$treatment)

#  assign new cutoff points
cutoffs_list$terrain_ruggedness_index_mean<-proposed_cutoffs_tri

## 4) ---- Elevation ---- 
# create histogramm
ggplot(data = matching_db)+
  geom_histogram(aes(elevation_mean,fill=as.factor(treatment)),alpha = 0.7, position="dodge")

table(cut(matching_db$elevation_mean,
          results_matching_PAs$breaks$elevation_mean),
      matching_db$treatment)

# we simplify by creating three different altitude groups. Lowland 0-500, midland 500-1500, highland >1500
# define proposed cuttoffs
proposed_cutoffs_elevation<-
  c(0,500,1500,max(matching_db$elevation_mean,na.rm = T))


# show the distribution in the proposed cutoffs 
table(cut(matching_db$elevation_mean,
          proposed_cutoffs_elevation),
      matching_db$treatment)



#  assign new cutoff points
cutoffs_list$elevation_mean<-proposed_cutoffs_elevation

## 5) ---- Forest ----
matching_db$fc_area_matchingyear

# create histogramm
ggplot(data = matching_db)+
  geom_histogram(aes(fc_area_matchingyear,fill=as.factor(treatment)),alpha = 0.7, position="dodge")

## -> Thsi graph could be used to justify argument of selection bias. Why matching?? 

# create table 
table(cut(matching_db$fc_area_matchingyear,
          results_matching_PAs$breaks$fc_area_matchingyear),
      matching_db$treatment)

# see how many have no forest cover at all
table(matching_db$fc_area_matchingyear==0,matching_db$treatment)


# proposed simplified cutoffs
  # 0 = 0% forest cover
  # >0 - 200 = >0-50 % forest cover
  # >200 - < 360 = >50% - 90% forest cover
  # 90-100% forest cover

# define proposed cuttoffs
proposed_cutoffs_fc<-
  c(-1,0,200,360,400)

# show the distribution in the proposed cutoffs 
table(cut(matching_db$fc_area_matchingyear,
          proposed_cutoffs_fc),
      matching_db$treatment)


#  assign new cutoff points
cutoffs_list$fc_area_matchingyear<-proposed_cutoffs_fc

## 6) ---- Forest Cover Loss ----
matching_db$average_fcl_matchingyear

# create histogramm
ggplot(data = matching_db)+
  geom_histogram(aes(average_fcl_matchingyear,fill=as.factor(treatment)),alpha = 0.7, position="dodge")

# create table
table(cut(matching_db$average_fcl_matchingyear,
          results_matching_PAs$breaks$average_fcl_matchingyear),
      matching_db$treatment)

# proposed simplified cutoffs: forest cover loss yes and no. 
# define proposed cuttoffs
proposed_cutoffs_fcl<-
  c(-1,0,400)

# show the distribution in the proposed cutoffs 
table(cut(matching_db$average_fcl_matchingyear,
          proposed_cutoffs_fcl),
      matching_db$treatment)


#  assign new cutoff points
cutoffs_list$average_fcl_matchingyear<-proposed_cutoffs_fcl

## 7) ---- Popgrowth----
sort(matching_db$average_popgrowth,decreasing = F)

# create histogramm
ggplot(data = matching_db)+
  geom_histogram(aes(average_popgrowth,fill=as.factor(treatment)),alpha = 0.7, position="dodge")

# create table
table(cut(matching_db$average_popgrowth,
          results_matching_PAs$breaks$average_popgrowth),
      matching_db$treatment)

# proposed simplified cutoffs:
  # note: original data is given in 1x1 km cells from worldpop grids. 

table(matching_db$average_popgrowth>10)
table(matching_db$average_popgrowth>1)
table(matching_db$average_popgrowth==0)
table(matching_db$average_popgrowth<0)
table(matching_db$average_popgrowth<(1*-1))
table(matching_db$average_popgrowth<(1*-10))

proposed_cutoffs_pop<-
  c(min(matching_db$average_popgrowth,na.rm = T),-10,-1,0,1,max(matching_db$average_popgrowth,na.rm = T))


# show the distribution in the proposed cutoffs 
table(cut(matching_db$average_popgrowth,
          proposed_cutoffs_pop),
      matching_db$treatment)


#  assign new cutoff points
cutoffs_list$average_popgrowth<-proposed_cutoffs_pop


# --- Rematch data -----
# show custom breakoffs
cutoffs_list

imbalance(
  matching_db$treatment,
  as.data.frame(matching_db),
  drop = c("treatment","UID","biome_max","country"))

results_matching_PAs_custom <-
  cem("treatment",
      as.data.frame(matching_db),
      drop =  c("treatment","UID","biome_max","country",
                "travel_time_to_nearby_cities_min_5k_10k",
                "travel_time_to_nearby_cities_min_50k_100k"),
      eval.imbalance = TRUE,cutpoints = cutoffs_list)

# check matched successes
results_matching_PAs_custom$tab

# check imbalance
results_matching_PAs_custom$imbalance

## compare both matchings

# check matched successes
results_matching_PAs$tab
results_matching_PAs_custom$tab

# check imbalance
results_matching_PAs$imbalance
results_matching_PAs_custom$imbalance

# ---- Check forest cover loss amongst matched and unmatched -----
# load data 
fcl_database<-
  read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/fcl_supported_AND_nonPas.csv")

keys_database<-
  read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/keys_wdpaid_bmz.csv")

projectdata_database<-
  read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/projectdata_supported.csv")


## WHERE TO FIND MATCHING PAIRS? -> THIS WOULD BE NEEDED 

# create a db of matched observations
matching_db_matched<-
  matching_db[which(results_matching_PAs_custom$matched==T),]

# in total 4529 matched obs. so that should be fine. However: More matched from treatment then non treatment (becasuse k2k is not enabled)
results_matching_PAs_custom$tab

results_matching_PAs_custom$group.idx
table(results_matching_PAs_custom$group.idx$G0==100)
table(results_matching_PAs_custom$group.idx$G1%in%results_matching_PAs_custom$group.idx$G0)

length(unique(results_matching_PAs_custom$strata))

str(results_matching_PAs_custom$strata)

sort(table(results_matching_PAs_custom$strata),decreasing = T)


## Regression Analysis


# NOTE: Possibly Interaction Treatment + Accessibility -> PAs that are closer are less protected then others.
# NOTE 2: Differentiate heterogenous treatment effects based on high deforestation - low deforestation

