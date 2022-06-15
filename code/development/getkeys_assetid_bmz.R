
##  Protected areas
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

wdpa_kfw <- wdpa_kfw %>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") %>%
  filter(STATUS != "Proposed") %>%
  filter(GEOMETRY_TYPE != "POINT") %>% 
  st_drop_geometry()

# Load link between assetid and WDPA ID
keys_assetid_wdpa <-
  read_sf("~/shared/datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_kfw.gpkg") %>%
  select(poly_id, WDPAID) %>%
  rename(".assetid" = "poly_id") %>%
  st_drop_geometry()

# load project data
kfw_projectdata <- read_delim("/datadrive/datalake/mapme.protectedareas/input/project-data/projectdata.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename("bmz_nummer" = "BMZNo",
         "community_involvement" = "Planned community involvement",
         "alternative_income" = "Planned creation of alternative income sources") %>% 
  select(bmz_nummer, community_involvement, alternative_income)


merged_id_bmz <- 
  left_join(keys_assetid_wdpa, keys_wdpaid_bmz,
                           by=c("WDPAID")) %>% 
  left_join(., kfw_projectdata,
            by=c("bmz_nummer")) %>% 
  left_join(., kfw_finance,
            by=c("bmz_nummer")) %>% 
  mutate(key = paste(.assetid, first_year)) %>% 
  distinct(key) %>% 
  select(-key)

write_csv(merged_id_bmz,
          "/datadrive/datalake/mapme.protectedareas/output/matching/model_frames/keys_assetid_bmz.csv")

test_dups <- merged_id_bmz %>% 
  mutate(key = paste(.assetid, first_year)) %>% 
  group_by(key) %>% 
  filter(n()>1)

# check whether some asset ids have multiple wdpa ids
check <- keys_assetid_wdpa %>% 
  mutate(key = paste(.assetid, WDPAID)) %>%
  distinct(key, .keep_all = T) %>% 
  group_by(.assetid) %>% 
  filter(n()>1)
  