library("tidyverse")
library("sf")
library("leaflet")
library("leaflet.extras")
library("leaflet.extras2")
library("ggsci")
library("scales")
library("htmltools")

# ----- load and transform protected areas data -----
##  Protected areas
wdpa_kfw<-
  read_sf("~/shared/datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg")

## create column for area coloring based on categories
wdpa_kfw$REP_AREA_cat<-
  cut(wdpa_kfw$REP_AREA,
      c(0,1000,5000,10000,20000,max(wdpa_kfw$REP_AREA)),
      c("< 1,000 sqkm","1,001-5,000 sqkm","5,001-10,000 sqkm","10,001-20,000 sqkm",paste("20,001-",max(wdpa_kfw$REP_AREA)," sqkm",sep="")))

# ----- load loss data and prepare for mapping -----
## loss statistics
gfw_lossstats<-
  read_csv("../../datalake/mapme.protectedareas/output/polygon/global_forest_watch/zonal_statistics_allPAs_long_temporal.csv")

## filter for area only
gfw_lossstats <-
  gfw_lossstats %>%
  filter(name == "area")

## load spatial data
wdpa_allPAs<-
  st_read("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_valid.gpkg")

## check if wdpa ids from loss statistics are contained in spatial data
table(unique(gfw_lossstats$WDPA_PID)%in%wdpa_allPAs$WDPA_PID) # all but one

## add column to spatial data that shows forest area in 2000 for each area (maximum is equal to area in 2000 since no forest gain is in the data)
max_forest <-
  gfw_lossstats %>%
  group_by(WDPA_PID) %>%
  dplyr::summarise(max_forest = max(value))

## merge to spatial and clean up
wdpa_allPAs <- 
  merge(wdpa_allPAs, max_forest,"WDPA_PID")

rm(max_forest)

## categorize maximum forest area by meaningfull and pretty breaks
wdpa_allPAs$max_forest_categorized <-
  cut(
    wdpa_allPAs$max_forest,
    breaks = c(-1, 100, 1000, 10000,100000, max(wdpa_allPAs$max_forest, na.rm = T)),
    labels = c("0-100", "101-1,000", "1,001-10,000", "10,001-100,000",">100,000")
  )

## calculate relative losses between 2000 and 2020 and add column to spatial data
relative_loss <-
  gfw_lossstats %>%
  group_by(WDPA_PID) %>%
  dplyr::summarise(relative_loss = 1-min(value)/max(value),absolute_loss = max(value)-min(value))

## merge to spatial and clean up
wdpa_allPAs <- 
  merge(wdpa_allPAs, relative_loss, "WDPA_PID")

rm(relative_loss)

## categorize relative loss by meaningfull and pretty breaks 
wdpa_allPAs$relative_loss_categorized <-
  cut(
    wdpa_allPAs$relative_loss,
    breaks = c(-1, 0.02, 0.05, 0.1, 0.2, 1),
    labels = c("0-2 %", "2-5 %", "5-10 %", "10-20 %",">20 %")
  )


## categorize absolute loss by meaningfull and pretty breaks 
wdpa_allPAs$absolute_loss_categorized <-
  cut(
    wdpa_allPAs$absolute_loss,
    breaks = c(-1, 10, 100, 1000, 10000,max(wdpa_allPAs$absolute_loss)),
    labels = c("0-10", "10-100", "100-1,000", "1,000-10,000",">10,000")
  )

## subset data and get centroids for map
wdpa_allPAs_lossdata<-wdpa_allPAs %>% 
  filter(DESIG_ENG != 'UNESCO-MAB Biosphere Reserve') %>% 
  filter(GEOMETRY_TYPE != 'POINT') %>% 
  filter(is.nan(relative_loss)!=TRUE) %>%
  st_centroid()

## subset all data for untreated
wdpa_nonSupportPA<-wdpa_allPAs %>% 
  filter(DESIG_ENG != 'UNESCO-MAB Biosphere Reserve') %>% 
  filter(GEOMETRY_TYPE != 'POINT') %>% 
  filter(is.na(bmz_n_1)==TRUE) 

## ----- create color pallets for the plots -----
# create colorramp function for area
# pal_relative_loss <- colorFactor(
#   palette = pal_ucscgb()(length(unique(wdpa_allPAs$relative_loss_categorized))),
#   domain = wdpa_allPAs$relative_loss_categorized
# )

pal_relative_loss <- colorFactor(
  palette = rev(brewer.pal(5, "RdYlGn")),
  domain = wdpa_allPAs$relative_loss_categorized
)

pal_absoute_loss <- colorFactor(
  palette = rev(brewer.pal(5, "RdYlGn")),
  domain = wdpa_allPAs$absolute_loss_categorized
)

# create colorramp function for country
colourCount = length(unique(wdpa_kfw$ISO3))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

pal_country <- colorFactor(
  palette = getPalette(colourCount),
  domain = wdpa_kfw$ISO3
)

# create colorramp2
# pal_treatment <- colorFactor(
#   palette = c("darkblue","orange"),
#   domain = matched_data_merged$treat_ever
# )

## add label string for map display 
wdpa_allPAs_lossdata$label <- 
  with(wdpa_allPAs_lossdata, paste(
  "<p> <b>", NAME, "</b> </br>",
  "Primary Forests (2000)",round(max_forest,digits=0)," ha", "</br>",
  "Absolute Loss (2000-2020):",round(absolute_loss,digits=0)," ha", "</br>",
  "Relative Loss (2000-2020):", round(relative_loss*100,digits=2)," %:",
  "</p>"))

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
  addPolygons(data = wdpa_kfw,opacity = 0.9,color = "orange", group = "PAs KfW-supported",label = ~htmlEscape(NAME),weight = 1)%>%
  addPolygons(data = wdpa_nonSupportPA,opacity = 0.7,color = "darkgrey", group = "PAs (Others)",label = ~htmlEscape(NAME),weight = 1)%>%
  # add own forest loss data as circles
  addCircleMarkers(data=wdpa_allPAs_lossdata,
                   color = ~pal_relative_loss(relative_loss_categorized),
                   group = "Relative Forest Loss (2001-2020)",
                   radius = ~ifelse(absolute_loss_categorized == ">10,000", 12, 
                                    ifelse(absolute_loss_categorized == "1,000-10,000", 6,
                                           ifelse(absolute_loss_categorized == "100-1,000", 4,
                                                  ifelse(absolute_loss_categorized == "10-100", 2,1
                                                  )      
                                           )
                                    )),
                   popup = ~label,
                   stroke = FALSE, 
                   fillOpacity = 0.5
                   ) %>%
  # add fullscreen control
  addFullscreenControl() %>%
  # add legent for area
  addLegend("bottomright",
            data = wdpa_allPAs,
            pal = pal_relative_loss,
            values = ~relative_loss_categorized,
            title = "Relative Forest Loss (2001-2020)",
            opacity = 1,
            group = "PA Area Size") %>% 
  # add layers control to define how and where data is displayed.
  addLayersControl(
    baseGroups = c("CartoDB","OpenStreetMap","Satellite","Topography","Nightlights"), #"Toner",,"Regional Primary Forests (2001)"
    overlayGroups = c("PAs KfW-supported","PAs (Others)","Relative Forest Loss (2001-2020)","Forest Cover Loss (2001-2020)", "Regional Primary Forests (2001)"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  # ommit certain layers
  hideGroup(group = c("PA Area Size","Forest Cover Loss (2001-2020)", "Regional Primary Forests (2001)"))

my_map

# ---- create horizontal barplot for 50 top absolute loss -----

wdpa_allPAs_lossdata$absolute_loss


wdpa_allPAs_lossdata %>%
  top_n(20, wdpa_allPAs_lossdata$absolute_loss) %>% 
  # arrange(desc(absolute_loss)) %>% 
  ggplot()+
  geom_bar(aes(x=reorder(NAME,-absolute_loss), y=absolute_loss,
               fill=PARENT_ISO,color=factor(bmz_n_1)),stat="identity",size = 1.1)+
  coord_flip()


wdpa_allPAs_lossdata

