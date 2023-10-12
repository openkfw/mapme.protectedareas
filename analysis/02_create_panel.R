library(tidyverse)
library(sf)

# --------- Load data ----------

#  Protected areas
wdpa_kfw<-
  read_sf("~/shared/datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

# Load KfW finance data
kfw_finance <-
  read_csv("~/shared/datalake/mapme.protectedareas/input/kfw_finance/mapme.protectedareas_kfw-finance-2021-03-17.csv") %>%
  filter(! is.na(bmz_nummer))

#Load wdpa bmz keys
keys_wdpaid_bmz <- read_csv("~/shared/datalake/mapme.protectedareas/output/matching/model_frames/keys_wdpaid_bmz.csv") %>%
  rename("bmz_nummer" = "value")

# merge wdpa_kfw with keys
wdpa_kfw <- left_join(wdpa_kfw, keys_wdpaid_bmz,
                      by=c("WDPAID"))

#add kfw_finance data to wdpa_kfw
wdpa_kfw <- left_join(wdpa_kfw, kfw_finance,
                      by=c("bmz_nummer"))

wdpa_year <- wdpa_kfw %>%
  dplyr::select(WDPAID, first_year) %>%
  st_drop_geometry

# Load link between assetid and WDPA ID
keys_assetid_wdpa <-
  read_sf("~/shared/datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_kfw.gpkg") %>%
  dplyr::select(poly_id, WDPAID) %>%
  rename(".assetid" = "poly_id") %>%
  st_drop_geometry()

keys_assetid_wdpa <- keys_assetid_wdpa %>%
  distinct(.assetid, WDPAID) %>%
  left_join(., wdpa_year,
            by=c("WDPAID")) %>%
  dplyr::select(.assetid, WDPAID, first_year)

# load WDPA area type
wdpa_type <-
  read_sf("~/shared/datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique_simplified.gpkg") %>%
  st_drop_geometry() %>%
  select("WDPAID", "NAME", "IUCN_CAT", "DESIG_ENG") %>%
  mutate(WDPA_strict=ifelse(IUCN_CAT=="II" | IUCN_CAT=="Ia" | IUCN_CAT=="Ib" | IUCN_CAT=="III" | IUCN_CAT=="IV", 1,
                            ifelse(IUCN_CAT=="V" | IUCN_CAT=="VI", 0, NA)))

# --------- List of matched datasets ----------

# Get list with all bmz pa combinations where data is available 
bmzpa_list <- list.files("/datadrive/kemmeng/paIE_useCase/data_output/matched_frame") %>% 
# bmzpa_list <- list.files("/datadrive/datalake/mapme.protectedareas/output/tabular/regression_input/CEM/tn_projects") %>% 
  grep("cem_tn_matched_data_*", ., value = TRUE) %>%
  gsub("cem_tn_matched_data_", "", .) %>% 
  # gsub(".rds", "", .)
  gsub(".gpkg", "", .)

# --------- Function to build panel ----------

panelize <- function(bmzpa_id) {
  
  # Keep only assetids that 
  keys <- keys_assetid_wdpa %>% 
    filter(WDPAID == substr(bmzpa_id, 11, 100))
  
  fyear <- kfw_finance %>% 
    filter(bmz_nummer == substr(bmzpa_id, 1, 9)) %>% 
    pull(first_year)
  
  # Load matched data
  m_data <- read_sf(paste0("/datadrive/kemmeng/paIE_useCase/data_output/matched_frame/cem_tn_matched_data_", bmzpa_id, ".gpkg")) %>%
    st_drop_geometry() %>% 
    # select(-c(starts_with(c("treecover", "treeloss")))) %>% 
    rename(".assetid" = "assetid",
           "treatment" = "group") %>% 
    rename_with(~ gsub("fc_percent", "treecover", .x, fixed = TRUE)) %>% 
    rename_with(~ gsub("fl_percent", "treeloss", .x, fixed = TRUE))
  
  # Merge with yearly data
  merged_data <- 
    m_data %>% 
    pivot_longer(cols = c(starts_with(c("treecover", "treeloss"))),
                 names_to = c(".value", "year"),
                 names_sep = "_") %>%
    rename("fc_area" = "treecover",
           "fc_loss" = "treeloss") %>%
    mutate(first_year = fyear,
           year = as.numeric(year),
           year_standard = year - first_year,
           weights_cem = weights) %>% 
    # left_join(., keys, # this can be deleted, looking at one PA at a time
    #           by=c(".assetid")) %>%
    mutate(WDPAID = ifelse(treatment == 0, 9999999999, substr(bmzpa_id, 11, 100))) %>% 
    # mutate(WDPAID = ifelse(treatment == 0, 9999999999, WDPAID)) %>% 
    filter(! is.na(year)) %>% 
    distinct(.assetid, year, .keep_all = TRUE) %>% # adjust this as soon as we have a better solution
    # mutate(treatment_disb = 1 * (treatment==1 & year_standard>=0),
    #        emissions_tha = emissions / 500)
    mutate(treatment_disb = 1 * (treatment==1 & year_standard>=0))
  
  # Export data
  write_csv(merged_data, 
            paste0("/datadrive/datalake/mapme.protectedareas/output/tabular/regression_input/CEM/tn_project_pa/cem_tn_matched_panel_", bmzpa_id, ".csv"))
}

# --------- Panelize data ----------

# for each item in bmzpa_list, function panelize is applied
map(bmzpa_list, function(x) panelize(x))

