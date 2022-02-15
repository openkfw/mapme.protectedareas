## Ecosystem_Services_GIZ
# author: Johannes Schielein
library("sf")
library("dplyr")
source("code/dopa-rest.R")

# read in polygons of interest
wdpa_giz_all = st_read("../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_giz_all_2019_wdpaV_Feb2022.gpkg")
wdpa_giz_supported = st_read("../../datalake/mapme.protectedareas/input/wdpa_giz/wdpa_")
wdpa_giz_nonsupported<-wdpa_giz_all %>% 
  filter(!WDPAID %in%wdpa_giz_supported$WDPAID)

# ----- Get Redlist data ----- 
# get redlist data of the PAs
df_redlist_status<-
  lapply(wdpa_giz_all$WDPAID, 
         FUN = get_redlist_status)%>% 
  bind_rows()

# see how many PAs from WDPA hat redlist data from DOPA
nrow(df_redlist_status)
length(unique(df_redlist_status$wdpa_id))

# see how many PAs of GIZ are covered
table(wdpa_giz_supported$WDPAID%in%df_redlist_status$wdpa_id)

View(df_redlist_status)
# save results
write_csv(df_redlist_status,
          "../../datalake/mapme.protectedareas/output/polygon/redlist/giz/wdpa_giz_2019_wdpaV_Feb2022/df_redlist_status_allPAs.csv")

# ----- Map of Threatened Species inside PAs
# aggregate the variable
df_redlist_status_aggregate <-
  df_redlist_status %>%
  group_by(wdpa_id) %>%
  summarise(
    redlist_total_species = sum(total_species),
    redlist_critically_endangered = sum(critically_endangered),
    redlist_endangered = sum(endangered)
  )
# match spatial data to redlist data
wdpa_giz_all_redlist<-
  merge(wdpa_giz_all,df_redlist_status_aggregate,by.x="WDPAID",by.y="wdpa_id")

# create categorized variables
quantile(wdpa_giz_all_redlist$redlist_total_species,c(0.2,0.4,0.6,0.8,1))

wdpa_giz_all_redlist$redlist_total_species_cat<-
  cut(wdpa_giz_all_redlist$redlist_total_species,
      c(0,250,500,750,1000,max(wdpa_kfw$REP_AREA)),
      c("< 250","251-500","501-750","751-1,000",paste("1,001-",max(wdpa_giz_all_redlist$redlist_total_species),sep="")))


table(wdpa_giz_all_redlist$redlist_total_species_cat)

quantile(wdpa_giz_all_redlist$redlist_critically_endangered,c(0.2,0.4,0.6,0.8,1))

wdpa_giz_all_redlist$redlist_critically_endangered_cat<-
  cut(wdpa_giz_all_redlist$redlist_critically_endangered,
      c(-1,1,2,5,10,max(wdpa_giz_all_redlist$redlist_critically_endangered)),
      c("0","1","2-5","6-10",paste("10-",max(wdpa_giz_all_redlist$redlist_critically_endangered),sep="")))

table(wdpa_giz_all_redlist$redlist_critically_endangered_cat)

## create color palettes
pal_total_cat <- colorFactor(
  palette = rev(brewer.pal(5, "RdYlGn")),
  domain = wdpa_giz_all_redlist$redlist_total_species_cat
)

## create color palettes
pal_critically_endangered_cat <- colorFactor(
  palette = rev(brewer.pal(5, "RdYlGn")),
  domain = wdpa_giz_all_redlist$redlist_critically_endangered_cat
)

## add label string for map display 
wdpa_giz_all_redlist$label <- 
  with(wdpa_giz_all_redlist, paste(
    "<p> <b>", NAME, "</b> </br>",
    "Species habitats (total)",redlist_total_species," species.", "</br>",
    "Species habitats (critically endangered):",redlist_critically_endangered," species,", "</br>",
    "Species habitats (endangered):", redlist_endangered," species.",
    "</p>"))

# get centroids for map
wdpa_giz_all_redlist<-st_make_valid(wdpa_giz_all_redlist)
sf_use_s2(FALSE)
wdpa_giz_all_redlist_centroid<-st_centroid(wdpa_giz_all_redlist)

# ----- create map -----
my_map <-
  leaflet() %>%
  # add external map providers
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$CartoDB.Positron, group="CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$Esri.WorldShadedRelief, group="Topography") %>%
  addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group="Nightlights") %>%
  addTiles(
    "https://tiles.globalforestwatch.org/umd_tree_cover_loss/latest/dynamic/{z}/{x}/{y}.png",
    group = "Forest Cover Loss (2001-2020)",
    #options=tileOptions(opacity = 0.7),
    attribution = "Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover Change.” Science 342 (15 November): 850–53. Data available on-line from: http://earthenginepartners.appspot.com/science-2013-global-forest."
  ) %>%
  addTiles(
    "https://tiles.globalforestwatch.org/umd_regional_primary_forest_2001/latest/dynamic/{z}/{x}/{y}.png",
    group = "Regional Primary Forests (2001)",
    attribution = "Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover Change.” Science 342 (15 November): 850–53. Data available on-line from: http://earthenginepartners.appspot.com/science-2013-global-forest."
  ) %>%
  # add own polygon data
  addPolygons(data = wdpa_giz_supported,opacity = 0.9,color = "red", group = "PAs GIZ-supported",label = ~htmlEscape(NAME),weight = 1)%>%
  # addPolygons(data = wdpa_giz_nonsupported,opacity = 0.7,color = "darkgrey", group = "PAs (Others)",label = ~htmlEscape(NAME),weight = 1)%>%
  # add own forest loss data as circles
  addCircleMarkers(data=wdpa_giz_all_redlist_centroid,
                   color = ~pal_critically_endangered_cat(redlist_critically_endangered_cat),
                   group = "Species Habitats (Redlist)",
                   radius = ~ifelse(redlist_total_species_cat == paste("1,001-",max(wdpa_giz_all_redlist_centroid$redlist_total_species),sep=""), 12, 
                                    ifelse(redlist_total_species_cat == "751-1,000", 6,
                                           ifelse(redlist_total_species_cat == "501-750", 4,
                                                  ifelse(redlist_total_species_cat == "251-500", 2,1
                                                  )      
                                           )
                                    )),
                   popup = ~label,
                   stroke = FALSE, 
                   fillOpacity = 0.5
  ) %>%
  # add fullscreen control
  addFullscreenControl() %>%
  # add legend(s)
  addLegend("bottomright",
            data = wdpa_giz_all_redlist_centroid,
            pal = pal_critically_endangered_cat,
            values = ~redlist_critically_endangered_cat,
            title = "Habitats of critically endangered species",
            opacity = 1,
            group = "Species Habitats (Redlist)") %>%
  
  # add layers control to define how and where data is displayed.
  addLayersControl(
    baseGroups = c("CartoDB","OpenStreetMap","Satellite","Topography","Nightlights"), #"Toner",,"Regional Primary Forests (2001)"
    overlayGroups = c("PAs GIZ-supported","PAs (Others)","Species Habitats (Redlist)","Forest Cover Loss (2001-2020)", "Regional Primary Forests (2001)"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  # ommit certain layers
  hideGroup(group = c("PA Area Size","Forest Cover Loss (2001-2020)", "Regional Primary Forests (2001)"))

my_map








