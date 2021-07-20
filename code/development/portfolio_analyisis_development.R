# development
library("tidyverse")
library("readxl")
library("janitor")
library("sf")
library("leaflet")
library("RColorBrewer")
library("plotly")
library("ggthemes")
library("scales")

# ----- load data -----
##  Protected areas
wdpa_kfw<-
  read_sf("data/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

#  Filter for supported areas
wdpa_kfw_treatment<-
  wdpa_kfw%>%
  filter(bmz_n_1>0)


# ----- TEOW Ecosystem analysis -----
# load data
db_teow <-
  rbind(
    read_csv(
      "~/shared/datalake/mapme.protectedareas/output/polygon/teow/teow_long_allPAs_merged_biome.csv"
    ),
    read_csv(
      "~/shared/datalake/mapme.protectedareas/output/polygon/teow/teow_long_allPAs_merged_eco.csv"
    )
  )

## (1) Data preparation ##
# clean and prepare database
db_teow_ecosystems <-
  db_teow %>%
  filter(grepl('teow_intersect_sqkm_', name))

db_teow_biomes <-
  db_teow %>%
  filter(grepl('biome_intersect_sqkm_', name))

# clean and prepare database for KfW areas only
db_teow_ecosystems_kfw <-
  db_teow %>%
  filter(grepl('teow_intersect_sqkm_', name)) %>%
  filter(WDPA_PID %in% wdpa_kfw_treatment$WDPAID)

db_teow_biomes_kfw <-
  db_teow %>%
  filter(grepl('biome_intersect_sqkm_', name)) %>%
  filter(WDPA_PID %in% wdpa_kfw_treatment$WDPAID)

# clean the names
db_teow_ecosystems_kfw$name<-
  gsub("teow_intersect_sqkm_","",db_teow_ecosystems_kfw$name)

db_teow_biomes_kfw$name<-
  gsub("biome_intersect_sqkm_","",db_teow_biomes_kfw$name)


# get the corresponding names for biomes and ecoregions
db_teow_complete<-
  read_sf("~/shared/datalake/mapme.protectedareas/input/teow/Terrestrial_Ecoregions_World_validated.gpkg")
# drop the geometry
db_teow_complete<-
  st_drop_geometry(db_teow_complete)

## (2) create an overview plot
# create summary table 
db_teow_biomes_kfw_summary <-
  db_teow_biomes_kfw %>%
  group_by(name) %>%
  summarize(area_sqkm = sum(value))

# add column if biome is Tropical & Subtropical Moist Broadleaf Forests 
db_teow_biomes_kfw_summary$biome<-
  ifelse(db_teow_biomes_kfw_summary$name=="Tropical & Subtropical Moist Broadleaf Forests",
         "Tropical & Subtropical Moist Broadleaf Forests",
         "Other Biomes")

biomes_plot <-
  na.omit(db_teow_biomes_kfw_summary) %>%
  ggplot() +
  geom_bar(aes(biome,
               area_sqkm, 
               fill=name), 
           stat = "identity", position = "stack") +
  labs(y = "Supported Area in sqkm", x = "", fill = "") +
  scale_fill_tableau()+
  theme_classic()+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_text(
    angle = 45))

ggplotly(biomes_plot)



## (3) create a treemap visualization of the supported ecoregions data ##
# create a joined table
db_fig_ecoregion <-
  db_teow_ecosystems_kfw %>%
  group_by(name) %>%
  summarize(value = round(sum(value),digits = 0)) %>%
  left_join(x = .,
            y= select(db_teow_complete, ECO_NAME, BIOME_NAME),
            by=c("name" = "ECO_NAME"))%>%
  distinct(.)




## 
# create an auxiliary table to join biomes (necessary dataformat for the function to work)
db_aux<-
  db_fig%>%
  group_by(BIOME_NAME)%>%
  summarize(value = round(sum(value),digits = 0)) 

# bind the two dataframes
db_fig <- rbind(db_fig,
                data.frame(
                  name = db_aux$BIOME_NAME,
                  value = db_aux$value,
                  BIOME_NAME = ""
                )) %>%
  filter(!is.na(name))
# plot the data
plot_ly(
  db_fig,
  labels = ~ name,
  parents = ~ BIOME_NAME,
  values = ~ value,
  type = "treemap",
  hovertemplate = "Ecosystem: %{label}<br>Area in sqkm: %{value}<extra></extra>"
)


# treemap plot with plotly: 
# see tutorial here https://plotly.com/r/treemaps/
# and https://stackoverflow.com/questions/60066748/how-can-i-plotly-a-ggplot-treemap
library("plotly")


fig <-
  db_teow_ecosystems %>%
  group_by(name) %>%
  summarize(value = round(sum(value),digits = 0)) %>%
  left_join(x = .,
            y= select(db_teow_complete, ECO_NAME, BIOME_NAME),
            by=c("name" = "ECO_NAME"))%>%
  distinct(.)%>%
  plot_ly(
    .,
    labels = ~ name,
    # ids = ~ BIOME_NAME,
    parents = NA,
    values = ~ value,
    marker=list(colorscale='Greens'),
    type = "treemap",
    hovertemplate = "Ecosystem: %{label}<br>Area in sqkm: %{value}<extra></extra>"
  )
fig

# 1.2 Total area tables and barplots
# rearrange data
db_fig <-
  db_fig %>%
  filter(!is.na(BIOME_NAME)) %>%
  filter(BIOME_NAME != "")%>%
  arrange(BIOME_NAME, value)
# factorize the ecoregions name
db_fig$name<-factor(db_fig$name, levels = db_fig$name)
# plot
ggplot(db_fig, aes(value, name, fill = BIOME_NAME)) +
  geom_bar(stat = "identity")+
  labs(y = "", x = "Supported ecoregion area in sqkm", fill = "") +
  theme_classic()

# string for calculating share of most relevant biome
round(filter(.data=db_teow_biomes_kfw_summary,name=="Tropical & Subtropical Moist Broadleaf Forests")$area_sqkm/sum(db_teow_biomes_kfw_summary$area_sqkm),digits = 0) 

# biomes numbers
db_teow_biomes_kfw%>%
  group_by(name)%>%
  summarize(area_sqkm=sum(value))

# ----- DOPA -----
dopa_raw<-
  read_excel("../../datalake/mapme.protectedareas/input/dopa/dopa_4-0/dopa_v4-0.xlsx",
             sheet = "Protected Areas",
             skip = 4,
             col_names = T, na = "#N/A")
#clean column names
dopa_raw<-
  clean_names(dopa_raw)
# Note: for a more detailed description on the variable names please open the original excel sheet

# check how many PAs from the KfW portfolio can be found in the dopa dataset
table(wdpa_kfw$WDPAID%in%dopa_raw$id)



table(wdpa_kfw_treatment$WDPAID%in%dopa_raw$id) # 54 FALSE and 344 TRUE - 13% are not inside dopa

# prepare data for join
dopa_raw$id<-as.character(dopa_raw$id) 

# join
dopa_kfw<-
  left_join(wdpa_kfw_treatment, dopa_raw, by=c("WDPA_PID" = "id"))

# The conservation of natural forests is one of the main policy goals of our financial support. In total KfW's financial support contributed to the protection of a forest area which extents over `r round(sum(dopa_kfw$tree_cover_km2,na.rm=T)/10^6, digits=3)` Mio km^2^ or `r round(sum(dopa_kfw$tree_cover_km2,na.rm=T)/357581, digits=1)` times the size of Germany. Most of the supported forests are situated in the Amazon basin where we cooperate with the governments of Bolivia, Brazil, Colombia, Ecuador and Peru. 

## Biomass
# Geodata can also tell us a bit about the relevance of conserving forests to mitigate global climate change. The following chart shows us, for example, how much carbon is stored in the vegetation and soils for supported protected areas. Conservation finance can help to keep this carbon in place that might be otherwise released to the atmosphere due to deforestation and forest degradation. 

# ```{r carbonplot, echo=FALSE, warning=FALSE}
biomass_long <-
  dopa_kfw %>%
  select(ISO3, total_mg_66, total_mg_71, total_mg_76) %>%
  st_drop_geometry() %>%
  pivot_longer(.,
               c(total_mg_66, total_mg_71,total_mg_76),
               names_to = "Carbon",
               values_to = "tonnes")

biomass_long$Carbon<-gsub("total_mg_66","Soil Organic Carbon",biomass_long$Carbon)
biomass_long$Carbon<-gsub("total_mg_71","Below Ground Carbon",biomass_long$Carbon)
biomass_long$Carbon<-gsub("total_mg_76","Above Ground Carbon",biomass_long$Carbon)

# group other countries
biomass_long$ISO3<-ifelse(biomass_long$ISO3=="BRA","Brazil","Other Countries")

carbon_plot <-
  na.omit(biomass_long) %>%
  ggplot() +
  geom_bar(aes(ISO3,
               round(tonnes / 10 ^ 9, digits = 2),
               fill = Carbon), stat = "identity") +
  labs(y = "Stored Carbon", x = "", fill = "") +
  theme_classic()
ggplotly(carbon_plot)

# save the plot outside
ggsave(
  filename = "output/plots/portfolio_analysis-carbo_plot.eps",
  plot = carbon_plot,
  width = 10,
  height = 8,
  units = "cm"
)

# ```

# As we can see the protected areas network in Latin America stores `r round((sum(biomass_long$tonnes,na.rm = T))/10^9,digits=2)` Gigatons of carbon that, if released completely to the atmosphere, would generate emissions which correspond to `r round(sum(biomass_long$tonnes,na.rm = T)*3.67/739000000,digits=0)` times the annual GHG emissions of Germany. Most of this carbon is stored in Brazil which is by far the largest country on the continent with the biggest network of protected areass. With the creation of this database and the use of open geodata we hope to be able to learn more about the effectiveness of our projects to reduce deforestation, the loss of biological diversity and to improve the livelihoods of people worldwide living in and around protected areas. 

# ----- Biodiversity -----
library("tidyverse")
library("ggthemes")

db_species <- 
  read_csv("/datadrive/datalake/mapme.protectedareas/output/polygon/dopa_rest/dopa_updated_results/species_list.csv")


db_redlist <- 
  read_csv("/datadrive/datalake/mapme.protectedareas/output/polygon/dopa_rest/dopa_updated_results/redlist_status.csv")

# create dataset for plot
db_redlist_iucn_status <-
  db_redlist %>%
  # filter and reshape
  select(-wdpa_pid, -total_species, -endemic, -threatened_endemic) %>%
  pivot_longer(!class, names_to = "status", values_to = "count") %>%
  # aggregate
  group_by(class, status) %>%
  summarize(value = round(sum(count),
                          digits = 0))
# reformat categories and factorize
redlist_categories<-c("data deficient",
                      "least concerned",
                      "near threatened", 
                      "threatened",
                      "vulnerable", 
                      "endangered", 
                      "critically endangered")
redlist_categories<-rev(redlist_categories)

db_redlist_iucn_status$status<-gsub("_"," ",db_redlist_iucn_status$status)
db_redlist_iucn_status$status<-factor(db_redlist_iucn_status$status,
                                      levels = redlist_categories)

# plot only threatened
db_redlist_iucn_status %>%
  filter(!status %in% c("data deficient", "least concerned")) %>%
  ggplot(.,
         aes(class, value, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "Nr. of Species", x = "Class", fill = "") +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  theme_bw()+
  theme(axis.text.x = element_text(
    angle = 45
  )) 

# # plot all
# db_redlist_iucn_status %>%
#   ggplot(.,
#          aes(class, value)) +
#   geom_bar(stat = "identity", position = "stack") +
#   labs(y = "", x = "Class", fill = "") +
#   theme_bw() +
#   scale_fill_stata()

# ----- dopa_carbonstorage -----
db_dopa <- 
  read_csv("/datadrive/datalake/mapme.protectedareas/output/polygon/dopa_rest/dopa_updated_results/multiple_indicators.csv")

colnames(db_dopa)
biomass_long <-
  db_dopa %>%
  select(wdpaid, iso3, agb_tot_c_mg, bgb_tot_c_mg, gsoc_tot_c_mg) %>%
  pivot_longer(.,
               c(agb_tot_c_mg, bgb_tot_c_mg, gsoc_tot_c_mg),
               names_to = "carbon",
               values_to = "value")

biomass_long$carbon<-gsub("agb_tot_c_mg","Soil Organic Carbon",biomass_long$carbon)
biomass_long$carbon<-gsub("bgb_tot_c_mg","Below Ground Carbon",biomass_long$carbon)
biomass_long$carbon<-gsub("gsoc_tot_c_mg","Above Ground Carbon",biomass_long$carbon)

carbon_plot <-
  na.omit(biomass_long) %>%
  # summarize per carbon type
  group_by(carbon,iso3) %>%
  summarize(value = round(sum(value / 10 ^ 9),digits = 2)) %>% #  convert to gigatons
  ggplot() +
  geom_bar(aes(iso3,
               value, 
               group = carbon,
               fill = carbon), 
           stat = "identity") +
  labs(y = "Stored Carbon (Gigatons)", x = "", fill = "") +
  scale_fill_tableau()+
  theme_classic()

ggplotly(carbon_plot)

length(unique(biomass_long$wdpaid)) # We have observations from 384 PAs


# ----- dopa xyz -----
colnames(db_dopa)
db_dopa$pop
# db_dopa$p_road_pa_perc_tot # road percentage in pa total

boxplot(db_dopa$p_population_bu_last_epoch_sum)
db_dopa$p_builtup_pa_sqkm # a lot of NAs

db_dopa$p_s

# ----- threats and landcover change -----
library("sf")
library(plotly)
library(tidyverse)
library(readr)

# Import dataset with information on wdpa
wdpa_info <- read_sf("data/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")
wdpa_info <- st_drop_geometry(wdpa_info)
wdpa_info <- wdpa_info %>%
  rename('wdpa_pid' = 'WDPA_PID') %>%
  mutate(wdpa_pid = as.character(wdpa_pid))


# Information on categories:
# https://www.iucn.org/theme/protected-areas/about/protected-area-categories
# Spalte: IUCN_CAT

# csvs accessibility
# "../../datalake/mapme.protectedareas/output/polygon/accessibility_to_cities/travel_time_to_nearby_cities_2000.csv"

wdpa_accessibility <- read_csv("../../datalake/mapme.protectedareas/output/polygon/accessibility_to_cities/travel_time_to_nearby_cities_2000.csv")
wdpa_accessibility <- wdpa_accessibility %>%
  rename('wdpa_pid' = 'WDPA_PID') %>%
  rename('travel_time_to_nearby_cities_min' = 'value') %>%
  mutate(wdpa_pid = as.character(wdpa_pid)) %>%
  select(wdpa_pid,travel_time_to_nearby_cities_min)

#aes(x,y, color=IUCN_CAT)

#scale_fill_color


wdpa_kfw_latinamerica <- read_csv("data/wdpa-kfw_latinamerica_2021-04-22.csv")
kfw_finance_complete <- read_csv("/datadrive/datalake/mapme.protectedareas/input/kfw_finance/mapme.protectedareas_kfw-finance_complete-2021-03-17.csv")
kfw_finance_aggregated <- read_csv("/datadrive/datalake/mapme.protectedareas/input/kfw_finance/mapme.protectedareas_kfw-finance-2021-03-17.csv")
gfw_zonalstats <- read_csv("/datadrive/datalake/mapme.protectedareas/output/polygon/global_forest_watch/zonal_statistics_supported_gfw_long.csv")


# new df with WDPA_ID and BMZ_no
merge1 <- wdpa_kfw_latinamerica %>%
  select(wdpa_pid, bmz_n_1) %>%
  rename('bmz_no'=starts_with("bmz"))

merge2 <- wdpa_kfw_latinamerica %>%
  select(wdpa_pid, bmz_n_2) %>%
  rename('bmz_no'=starts_with("bmz"))

merge3 <- wdpa_kfw_latinamerica %>%
  select(wdpa_pid, bmz_n_3) %>%
  rename('bmz_no'=starts_with("bmz"))

merge4 <- wdpa_kfw_latinamerica %>%
  select(wdpa_pid, bmz_n_4) %>%
  rename('bmz_no'=starts_with("bmz"))

merge5 <- wdpa_kfw_latinamerica %>%
  select(wdpa_pid, bmz_n_5) %>%
  rename('bmz_no'=starts_with("bmz"))

# df with WDPA_ID and BMZ_No (multiple entries for a single wdpa_id possible!)
merged_wdpa_bmz <- mget(ls(pattern="^merge")) %>%
  bind_rows() %>%
  na.omit()

# merge with gfw data
gfw_zonalstats <- gfw_zonalstats %>%
  rename('wdpa_pid'='WDPA_PID') %>%
  mutate(wdpa_pid = as.character(wdpa_pid))

gfw_zonalstats_with_bmzno <- left_join(merged_wdpa_bmz, gfw_zonalstats, 
                                       by=c("wdpa_pid"))


# merge with kfw finance data
kfw_finance_aggregated <- kfw_finance_aggregated %>%
  rename('bmz_no'=starts_with("bmz"))

kfw_finance_with_wdpa <- left_join(merged_wdpa_bmz, kfw_finance_aggregated,
                                   by=c('bmz_no'))

# merge kfw finance data and gfw

gfw_kfw <- full_join(gfw_zonalstats_with_bmzno, kfw_finance_aggregated,
                     by=c('bmz_no'))

area_2000_values <- gfw_zonalstats %>%
  filter(name=='area_2000') %>%
  mutate(name = NULL) %>%
  rename('start_value' = 'value')

gfw_kfw_area_pct <- left_join(gfw_kfw, area_2000_values,
                              by=c('wdpa_pid')) %>%
  filter(str_detect(name, 'area')) %>%
  mutate(area_pct = value/start_value) %>%
  drop_na(wdpa_pid)

# area relative to project start
gfw_kfw_area_pct_relyear <- gfw_kfw_area_pct %>%
  mutate(area_year = as.numeric(substr(name,6,10)),
         rel_year = area_year - first_year)


# Merge with IUCN Categories

gfw_kfw_area_pct_relyear_IUCN <- left_join(gfw_kfw_area_pct_relyear,wdpa_info,
                                           by=c('wdpa_pid')) %>%
  select(wdpa_pid, bmz_no, area_pct, area_year, rel_year, IUCN_CAT)


# Merge with Accesibility

gfw_kfw_area_pct_relyear_access <- left_join(gfw_kfw_area_pct_relyear, wdpa_accessibility,
                                             by=c('wdpa_pid')) %>%
  select(wdpa_pid, bmz_no, area_pct, area_year, rel_year, travel_time_to_nearby_cities_min)

# Plots

#all in one
# p2 <- gfw_kfw_area_pct_relyear %>%
#   filter(bmz_no == 209810961) %>%
#   ggplot()+
#   geom_line(aes(x = rel_year, y = area_pct, group=interaction(wdpa_pid, bmz_no)))+
#   theme(axis.text.x=element_text(angle = 90, hjust = 0))+
#   geom_vline(xintercept = 0,
#              linetype = 'dotted',
#              color = 'red')
# ggplotly(p2)


# All BMZ numbers in a grid, area as a fraction relative to area in 2000, relative to start of project
# p3 <- gfw_kfw_area_pct_relyear %>%
#   ggplot()+
#   geom_line(aes(x = rel_year, y = area_pct, group=interaction(wdpa_pid, bmz_no)))+
#   theme(axis.text.x=element_text(angle = 90, hjust = 0))+
#   geom_vline(xintercept = 0,
#              linetype = 'dotted',
#              color = 'red')+
#   ylim(0.75,1)+
#   facet_wrap(~ bmz_no, nrow = 4)
# ggplotly(p3)

# With IUCN Categories
p4 <- gfw_kfw_area_pct_relyear_IUCN %>%
  ggplot()+
  geom_line(aes(x = rel_year, y = area_pct, group=interaction(wdpa_pid, bmz_no), color=IUCN_CAT))+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  geom_vline(xintercept = 0,
             linetype = 'dotted',
             color = 'red')+
  ylim(0.75,1)+
  facet_wrap(~ bmz_no, nrow = 4)+
  labs(y = "Forst Cover Loss (baseline 2000)", x = "Years before/after project start", fill = "Traveltime to major city") +
  scale_colour_manual(values=brewer.pal(n = 10,name = "Paired"),
                      name="IUCN Categories",
                      labels = c("Ia Strict Nature Reserve", 
                                 "Ib Wilderness Area", 
                                 "II National Park",
                                 "III Natural Monument or Feature",
                                 "IV Habitat/Species Management Area",
                                 "Not Applicable",
                                 "Not Assigned",
                                 "Not Reported",
                                 "V Protected Landscape/ Seascape",
                                 "VI Protected area with sustainable use of natural resources"))
ggplotly(p4)

# With Accessibility
# p5 <- gfw_kfw_area_pct_relyear_access %>%
#   ggplot()+
#   geom_line(aes(x = rel_year, y = area_pct, group=interaction(wdpa_pid, bmz_no), color=travel_time_to_nearby_cities_min))+
#   theme(axis.text.x=element_text(angle = 90, hjust = 0))+
#   geom_vline(xintercept = 0,
#              linetype = 'dotted',
#              color = 'red')+
#   ylim(0.75,1)+
#   scale_colour_brewer()+
#   facet_wrap(~ bmz_no, nrow = 4)
# ggplotly(p5)

# With Accessibility, accessibility values > 'cutoff_minutes' are set to 'cutoff_minutes'
# e.g. values > 1000 are set to 1000 to make the colorscale more meaningful
gfw_kfw_area_pct_relyear_access$travel_time_to_nearby_cities_min_cat<-
  cut(gfw_kfw_area_pct_relyear_access$travel_time_to_nearby_cities_min, 
      breaks=c(-Inf, 60, 120, 180, 240, Inf), 
      labels=c("< 1hr","1-2 hrs.","2-3 hrs.","3-4 hrs.",">4 hrs."))


p6 <- gfw_kfw_area_pct_relyear_access %>%
  na.omit(gfw_kfw_area_pct_relyear_access)%>%
  ggplot()+
  geom_line(aes(x = rel_year, 
                y = area_pct, 
                color=travel_time_to_nearby_cities_min_cat,
                group=interaction(wdpa_pid, 
                                  bmz_no
                )))+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))+
  geom_vline(xintercept = 0,
             linetype = 'dotted',
             color = 'red')+
  ylim(0.75,1)+
  labs(y = "Forst Cover Loss (baseline 2000)", x = "Years before/after project start", fill = "Traveltime to major city") +
  scale_colour_manual(values = c("red", "orange", "yellow","green","darkgreen"))+
  facet_wrap(~ bmz_no, nrow = 5)

ggplotly(p6)






colnames(db_dopa)
# carbon_tot_c_mg
# histogramm population densities
# water surface
# gfc_treecover_land_sqkm


