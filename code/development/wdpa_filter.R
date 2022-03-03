# filter
wdpa_LA<-
  st_read("../../datalake/mapme.protectedareas/input/wdpa-kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_valid_simplified.gpkg")

wdpa_LA <-
  wdpa_LA %>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") %>%
  filter(STATUS != "Proposed") %>%
  filter(GEOMETRY_TYPE != "POINT")