
rm(list=ls())
options(scipen = 999)
# get packages
lop <- c("dplyr", "plm", "stargazer", "tidyverse", "cem", "multiwayvcov", "modelsummary", "fixest")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)



#### ---- Create table for all years -----
all_years <- c(2004:2016, 2019)
for (t in all_years) {
  print(t)
  panel.df <- read.csv(paste0("/datadrive/datalake/mapme.protectedareas/output/tabular/regression_input/CEM/cem_matched_panel_", t, ".csv"), sep = ",", stringsAsFactors = F)
  
  # create time varying treatment variable
  panel.df <- panel.df %>% 
    mutate(treatment_disb = (treatment==1 & year_standard>=0),
           emissions_kg = emissions*1000000*500) %>% 
    rename(., weights_cem=weights) # rename weights
  
  assign(paste0("m1_",t), feols(fc_loss ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid) )
  assign(paste0("m2_",t), feols(fc_area ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid) )
  
  
  feols(emissions ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid)
  feols(emissions_kg ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid)
  
  feols(log(emissions_kg) ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid)
  
  feols(fc_loss ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid)
  feols(log(fc_loss) ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid)
  
  
}
m_list1 <- list(m1_2004, m1_2005, m1_2006, m1_2007, m1_2008, m1_2009, m1_2010, m1_2011, m1_2011, m1_2012, m1_2014, m1_2015, m1_2016, m1_2019)
m_list2 <- list(m2_2004, m2_2005, m2_2006, m2_2007, m2_2008, m2_2009, m2_2010, m2_2011, m2_2011, m2_2012, m2_2014, m2_2015, m2_2016, m2_2019)


names(m_list1) <- all_years
names(m_list2) <- all_years

# Output: fc_loss
modelsummary(m_list1,
             output = "/datadrive/datalake/mapme.protectedareas/output/tabular/regression_output/table1.html",
             coef_rename = c("fc_loss" = "Forest cover loss", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Dependent variable: Forest cover loss")
)

# Output: fc_area
modelsummary(m_list2,
             output = "/datadrive/datalake/mapme.protectedareas/output/tabular/regression_output/table2.html",
             coef_rename = c("fc_area" = "Forest cover area", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Dependent variable: Forest cover area")
)

modelplot(m_list1)



#### ---- Create table for all years -----

## WDPA data
wdpa <- read_csv("../../datalake/mapme.protectedareas/processing/fishnet/honeycomb_5_sqkm_subset_intersect_wdpa_long.csv") %>% 
  rename(.assetid=poly_id) %>% 
  select(.assetid, WDPAID) %>% 
  distinct


panel_new.df <- merge(panel.df, wdpa, all = T)



t=2006
assign(paste0("m1_",t), feols(fc_loss ~ treatment_disb | .assetid + year, data = panel_new.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid) )







#### ---- Create table for case study Colombia-----
t <- 2015
panel.df <- read.csv(paste0("/datadrive/datalake/mapme.protectedareas/output/tabular/regression_input/CEM/cem_matched_panel_", t, ".csv"), sep = ",", stringsAsFactors = F)

# create time varying treatment variable
panel_br.df <- panel.df %>% 
  mutate(treatment_disb = (treatment==1 & year_standard>=0)) %>% 
  rename(., weights_cem=weights)  %>% 
  subset(NAME_0=="Brazil")


m1 <- feols(fc_loss ~ treatment_disb | .assetid + year, data = panel_br.df, weights = panel_br.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid)

m2 <- feols(fc_area ~ treatment_disb | .assetid + year, data = panel_br.df, weights = panel_br.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid)


m_list_br <- list(m1, m2)

modelsummary(dvnames(m_list_br),
             output = "/datadrive/datalake/mapme.protectedareas/output/tabular/regression_output/table_br.html",
             coef_rename = c("fc_loss" = "Forest cover loss", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Matching frame 2015: Brazil only")
)


modelsummary(dvnames(m_list_br),
             coef_rename = c("fc_loss" = "Forest cover loss", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Matching frame 2015: Brazil only")
)








