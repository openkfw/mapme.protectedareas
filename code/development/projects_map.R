library("tidyverse")
library("sf")
library("leaflet")
library("leaflet.extras")
library("leaflet.extras2")
library("ggsci")
library("scales")
library("htmltools")

##  Protected areas
wdpa_kfw<-
  read_sf("~/shared/datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

## create column for area coloring
wdpa_kfw$REP_AREA_cat<-
  cut(wdpa_kfw$REP_AREA,
      c(0,1000,5000,10000,20000,max(wdpa_kfw$REP_AREA)),
      c("< 1,000 sqkm","1,001-5,000 sqkm","5,001-10,000 sqkm","10,001-20,000 sqkm",paste("20,001-",max(wdpa_kfw$REP_AREA)," sqkm",sep="")))


## fishnet
fishnet <-
  read_sf(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_all_update_Dec-07.gpkg"
  )
# load matching frame data
# matched_data<-rbind(read.csv("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_2015.csv"),
#                     read.csv("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_2007.csv"))

matched_data<-read.csv("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_2015.csv")
# filter to delete multiple observations
matched_data<-
  matched_data %>%
  filter(year == 2015)
# merge data 
matched_data_merged<-
  merge(fishnet, matched_data, "poly_id")

matched_data_merged<-st_transform(matched_data_merged,crs = st_crs(wdpa_kfw))

## Create Color Pals for the plot data
# create colorramp function for area
pal_area <- colorFactor(
  palette = pal_npg("nrc")(length(unique(wdpa_kfw$REP_AREA_cat))),
  domain = wdpa_kfw$REP_AREA_cat
)

# create colorramp function for country
colourCount = length(unique(wdpa_kfw$ISO3))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

pal_country <- colorFactor(
  palette = getPalette(colourCount),
  domain = wdpa_kfw$ISO3
)

# create colorramp2
pal_treatment <- colorFactor(
  palette = c("darkblue","orange"),
  domain = matched_data_merged$treat_ever
)

## Crate map
# wdpa_kfw_treatment_centroid<-st_transform(st_centroid(wdpa_kfw_treatment_centroid),crs = 4326)
my_map <-
  leaflet() %>%
  # add external map providers
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$CartoDB.Positron, group="CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$Esri.WorldShadedRelief, group="Topography") %>%
  addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group="Nightlights") %>%
  addTiles("https://tiles.globalforestwatch.org/umd_tree_cover_loss/latest/dynamic/{z}/{x}/{y}.png",
           group="Forest Cover Loss (2001-2020)",
           #options=tileOptions(opacity = 0.7),
           attribution = "Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover Change.” Science 342 (15 November): 850–53. Data available on-line from: http://earthenginepartners.appspot.com/science-2013-global-forest.")%>%
#  addTiles("https://tiles.globalforestwatch.org/umd_regional_primary_forest_2001/latest/dynamic/{z}/{x}/{y}.png",group="Regional Primary Forests (2001)",attribution = "Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover Change.” Science 342 (15 November): 850–53. Data available on-line from: http://earthenginepartners.appspot.com/science-2013-global-forest.")%>%
  # add own data
  addPolygons(data = wdpa_kfw,opacity = 0.9,color = "orange", group = "PA Boundaries (all years)",label = ~htmlEscape(NAME),weight = 1)%>%
  addPolygons(data = wdpa_kfw,opacity = 1,color = ~pal_area(REP_AREA_cat), group = "PA Area Size",label = ~htmlEscape(REP_AREA),weight = 1)%>%
  addPolygons(data = wdpa_kfw,opacity = 1,color = ~pal_country(ISO3), group = "Country",label = ~htmlEscape(ISO3),weight = 1)%>%
  addPolygons(data = matched_data_merged, opacity = 0.9,color = ~pal_treatment(treat_ever), group = "Cells (Treamtent & Control) in 2015",label = ~htmlEscape(wdpa_id),weight = 1)%>%
  # fullscreen control
  addFullscreenControl() %>%
  # add legent for area
  addLegend("bottomright",
            data = wdpa_kfw,
            pal = pal_area,
            values = ~REP_AREA_cat,
            title = "Total Reported Area",
            opacity = 1,
            group = "PA Area Size") %>% 
  addLegend("bottomright",
            data = matched_data_merged,
            pal = pal_treatment,
            values = ~treat_ever,
            title = "Treatement",
            opacity = 1,
            group = "Cells (Treamtent & Control) in 2015") %>% 
  addLegend("bottomright",
            data = wdpa_kfw,
            pal = pal_country,
            values = ~ISO3,
            title = "Country",
            opacity = 1,
            group = "Country") %>% 
  # add layers control to define which data is shown or ommited in default view
  addLayersControl(
    baseGroups = c("CartoDB","OpenStreetMap","Satellite","Topography","Nightlights"), #"Toner",,"Regional Primary Forests (2001)"
    overlayGroups = c("PA Boundaries (all years)","Country","PA Area Size","Cells (Treamtent & Control) in 2015","Forest Cover Loss (2001-2020)"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  # ommit certain layers
  hideGroup(group = c("Country","PA Area Size","Cells (Treamtent & Control) in 2015","Forest Cover Loss (2001-2020)"))

my_map


# check which polygons are classified as controls but are inside a PA
matched_data_merged%>%
  #filter(!is.na(wdpa_id))%>%
  filter(treat_ever==0)
