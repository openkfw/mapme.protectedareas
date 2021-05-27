# development
library("tidyverse")

# Financial data
  # How much money was spend over time and different countries?



# ----- TEOW Ecosystem analysis -----
sort(unique(db_teow$name))

## Questions to be responded
  # 1. Overview of supported ecosystems (total area)
  # 2. Overview of supported biomes (total area)
  # 3. How did our support for different biomes change over time? (total area)

# 1.1 ---- Total area treemaps
# clean and prepare database
db_teow_ecosystems <-
  db_teow %>%
  filter(grepl('teow_intersect_sqkm_', name)) %>%
  filter(WDPAID %in% wdpa_kfw_treatment$WDPAID)

db_teow_ecosystems$name<-
  gsub("teow_intersect_sqkm_","",db_teow_ecosystems$name)

db_teow_biomes <-
  db_teow %>%
  filter(grepl('biome_intersect_sqkm_', name)) %>%
  filter(WDPAID %in% wdpa_kfw_treatment$WDPAID)

# get the corresponding names for biomes and ecoregions
db_teow_complete<-
  read_sf("~/shared/datalake/mapme.protectedareas/input/teow/Terrestrial-Ecoregions-World.gpkg")
# drop the geometry
db_teow_complete<-
  st_drop_geometry(db_teow_complete)

# join 
db_fig <-
  db_teow_ecosystems %>%
  group_by(name) %>%
  summarize(value = round(sum(value),digits = 0)) %>%
  left_join(x = .,
            y= select(db_teow_complete, ECO_NAME, BIOME_NAME),
            by=c("name" = "ECO_NAME"))%>%
  distinct(.)

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





