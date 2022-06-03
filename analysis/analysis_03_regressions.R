
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
           emissions_kg = emissions*1000000*500,
           NAME_0_num = as.numeric(as.factor(NAME_0))) %>% 
    rename(., weights_cem=weights) # rename weights
  # create WDPA cluster that are country specific (important for control group, as all controll cells belong to ID 9999999999 regardless of the country. Now the control cells are clustered for each country.)
  panel.df$WDPAID_cluster <- as.numeric(paste0(panel.df$NAME_0_num, panel.df$WDPAID)) 
  
  assign(paste0("m11_",t), feols(fc_loss ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid) )
  assign(paste0("m21_",t), feols(fc_area ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid) )
  
  assign(paste0("m12_",t), feols(fc_loss ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~WDPAID_cluster) )
  assign(paste0("m22_",t), feols(fc_area ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~WDPAID_cluster) )
  
  assign(paste0("m32_",t), feols(emissions ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~WDPAID_cluster) )
  
  
  
  
}
m_list1_compare <- list(m11_2004, m12_2004, m11_2005, m12_2005, m11_2006, m12_2006, m11_2007, m12_2007, m11_2008, m12_2008, m11_2009, m12_2009, m11_2010, m12_2010, m11_2011, m12_2011, m11_2011, m12_2011, m11_2012, m12_2012, m11_2014, m12_2014, m11_2015, m12_2015, m11_2016, m12_2016, m11_2019, m12_2019)
m_list2_compare <- list(m21_2004, m22_2004, m21_2005, m22_2005, m21_2006, m22_2006, m21_2007, m22_2007, m21_2008, m22_2008, m21_2009, m22_2009, m21_2010, m22_2010, m21_2011, m22_2011, m21_2011, m22_2011, m21_2012, m22_2012, m21_2014, m22_2014, m21_2015, m22_2015, m21_2016, m22_2016, m21_2019, m22_2019)

m_list1 <- list(m12_2004, m12_2005, m12_2006, m12_2007, m12_2008, m12_2009, m12_2010, m12_2011, m12_2011, m12_2012, m12_2014, m12_2015, m12_2016, m12_2019)
m_list2 <- list(m22_2004, m22_2005, m22_2006, m22_2007, m22_2008, m22_2009, m22_2010, m22_2011, m22_2011, m22_2012, m22_2014, m22_2015, m22_2016, m22_2019)
m_list3 <- list(m32_2004, m32_2005, m32_2006, m32_2007, m32_2008, m32_2009, m32_2010, m32_2011, m32_2011, m32_2012, m32_2014, m32_2015, m32_2016, m32_2019)


names(m_list1_compare) <- sort(c(all_years, all_years))
names(m_list2_compare) <- sort(c(all_years, all_years))

names(m_list1) <- all_years
names(m_list2) <- all_years
names(m_list3) <- all_years

# Output: fc_loss
modelsummary(m_list1,
             output = "/datadrive/datalake/mapme.protectedareas/output/tabular/regression_output/table1_cluster.html",
             coef_rename = c("fc_loss" = "Forest cover loss", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Dependent variable: Forest cover loss")
)

modelsummary(m_list1_compare,
             output = "/datadrive/datalake/mapme.protectedareas/output/tabular/regression_output/table1_cluster_compare.html",
             coef_rename = c("fc_loss" = "Forest cover loss", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Dependent variable: Forest cover loss")
)

# Output: fc_area
modelsummary(m_list2,
             output = "/datadrive/datalake/mapme.protectedareas/output/tabular/regression_output/table2_cluster.html",
             coef_rename = c("fc_area" = "Forest cover area", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Dependent variable: Forest cover area")
)

modelsummary(m_list2_compare,
             output = "/datadrive/datalake/mapme.protectedareas/output/tabular/regression_output/table2_cluster_compare.html",
             coef_rename = c("fc_area" = "Forest cover area", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Dependent variable: Forest cover area")
)

# Output: emissions
modelsummary(m_list3,
             output = "/datadrive/datalake/mapme.protectedareas/output/tabular/regression_output/table3.html",
             coef_rename = c("emissions" = "Emissions from deforestation", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Dependent variable: Forest cover area")
)


b <- list(geom_vline(xintercept = 0, color = 'red'))

modelplot(m_list1, background = b)

modelplot(m_list2, background = b)




#### ---- Create table for case study Colombia-----
t <- 2015
panel.df <- read.csv(paste0("/datadrive/datalake/mapme.protectedareas/output/tabular/regression_input/CEM/cem_matched_panel_", t, ".csv"), sep = ",", stringsAsFactors = F)

# create time varying treatment variable
panel.df <- panel.df %>% 
  mutate(treatment_disb = (treatment==1 & year_standard>=0),
         emissions_kg = emissions*1000000*500,
         NAME_0_num = as.numeric(as.factor(NAME_0))) %>% 
  rename(., weights_cem=weights) %>% 
  subset(NAME_0=="Brazil")

# create WDPA cluster that are country specific (important for control group, as all controll cells belong to ID 9999999999 regardless of the country. Now the control cells are clustered for each country.)
panel.df$WDPAID_cluster <- as.numeric(paste0(panel.df$NAME_0_num, panel.df$WDPAID)) 


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








