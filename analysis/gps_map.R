---
  title: "GPS Testmap"
author: "Johannes Schielein (Author)"
date: "2021-12-22"
output: workflowr::wflow_html
editor_options:
  chunk_output_type: console
---
  
```{r setup, message = FALSE,warning = FALSE,include = FALSE}
# load relevant libraries
library("tidyverse")
library("sf")
library("leaflet")
library("leaflet.extras")
library("leaflet.extras2")
```


```{r projectmap, echo=FALSE, warning=FALSE,  fig.width=8}
map <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron, group="CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$Esri.WorldShadedRelief, group="Topography") %>%
  addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group="Nightlights") %>%
# add layers control to define which data is shown or ommited in default view
  addLayersControl(
  baseGroups = c("CartoDB","OpenStreetMap","Satellite","Topography","Nightlights"))

map <- addControlGPS(map, options = gpsOptions(position = "topleft", activate = TRUE, 
                                               autoCenter = TRUE, maxZoom = 10, 
                                               setView = TRUE))
activateGPS(map)
```