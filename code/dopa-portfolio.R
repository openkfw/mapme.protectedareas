# Script: dopa-portfolio.R
# Purpose: Script for analyzing KfW PA portfolio with DOPA data
# Author: Johannes Schielein
# Last update: Q1 2021

# ----- read in data, clean and join relevant variables ----- 
# get a clean list of column names
dopa_raw<-
  read_excel("../../datalake/mapme.protectedareas/input/dopa/dopa_4-0/dopa_v4-0.xlsx",
             sheet = "Protected Areas",
             skip = 4,
             col_names = T, na = "#N/A")
#clean column names
dopa_raw<-
  clean_names(dopa_raw)

# Note: for a more detailed description on the variable names please open the original excel sheet

# load supported areas
wdpa_kfw<-
  read_sf("../../datalake/mapme.protectedareas/output/wdpa-kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs.gpkg")

# check how many PAs from the KfW portfolio can be found in the dopa dataset
table(wdpa_kfw$WDPA_PID%in%dopa_raw$id)
# Note: 372 PAs could be detected and 61 not. At this stage DOPA can therefore only serve to analyze 85% of KfW Portfolio. 

# prepare data for join
dopa_raw$id<-as.character(dopa_raw$id) 

# join
dopa_kfw<-
  left_join(wdpa_kfw, dopa_raw, by=c("WDPA_PID" = "id"))

# ----- reduce columns and rename for analysis ----
# soil organic carbon
# below ground carbon
# above ground carbon
# carbon as sum

# land cover classes 

# land degradation

# People inside PA
# People around PA

# Tree cover

# permanent surface water
# seasonal surface water





