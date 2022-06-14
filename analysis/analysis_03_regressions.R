
rm(list=ls())
options(scipen = 999)
# get packages
lop <- c("dplyr", "plm", "stargazer", "tidyverse", "cem", "multiwayvcov", "modelsummary", "fixest")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)



#### ---- Create table for all years -----
all_years <- c(2004:2017, 2019)

# Prepare observations table
obs_table <- matrix(NA, nrow = 3, ncol = length(all_years)+1)
colnames(obs_table) <- c("name", all_years)
# rownames(obs_table) <- c("CEM obs total", "--obs treat", "--obs control")
obs_table[1,1] <- "CEM obs total"
obs_table[2,1] <- "--obs treat"
obs_table[3,1] <- "--obs control"

obs_table_m11 <- obs_table
obs_table_m21 <- obs_table
obs_table_m12 <- obs_table
obs_table_m22 <- obs_table
obs_table_m32 <- obs_table


for (t in all_years) {
  print(t)
  panel.df <- read.csv(paste0("/datadrive/datalake/mapme.protectedareas/output/tabular/regression_input/CEM/cem_matched_panel_", t, ".csv"), sep = ",", stringsAsFactors = F)
  
  # create time varying treatment variable
  panel.df <- panel.df %>% 
    mutate(treatment_disb = (treatment==1 & year_standard>=0),
           emissions_tha = emissions/500,
           NAME_0_num = as.numeric(as.factor(NAME_0))) %>% 
    rename(., weights_cem=weights) # rename weights
  # create WDPA cluster that are country specific (important for control group, as all controll cells belong to ID 9999999999 regardless of the country. Now the control cells are clustered for each country.)
  panel.df$WDPAID_cluster <- as.numeric(paste0(panel.df$NAME_0_num, panel.df$WDPAID)) 
  
  # assign(paste0("m11_",t), feols(fc_loss ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid) )
  # assign(paste0("m21_",t), feols(fc_area ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~.assetid) )
  
  assign(paste0("m12_",t), feols(fc_loss ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~WDPAID_cluster) )
  assign(paste0("m22_",t), feols(fc_area ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~WDPAID_cluster) )
  
  assign(paste0("m32_",t), feols(emissions_tha ~ treatment_disb | .assetid + year, data = panel.df, weights = panel.df$weights_cem, panel.id = ~.assetid+year, cluster = ~WDPAID_cluster) )
  
  
  
  # # m11
  # obs_table_m11[1,paste0(t)] <- nrow(panel.df[get(paste0("m11_",t))$obs_selection$obsRemoved,])/regyears
  # obs_table_m11[2,paste0(t)] <- table(panel.df[get(paste0("m11_",t))$obs_selection$obsRemoved,]$treatment)[2]/regyears
  # obs_table_m11[3,paste0(t)] <- table(panel.df[get(paste0("m11_",t))$obs_selection$obsRemoved,]$treatment)[1]/regyears
  # 
  # # m21
  # obs_table_m21[1,paste0(t)] <- nrow(panel.df[get(paste0("m21_",t))$obs_selection$obsRemoved,])/regyears
  # obs_table_m21[2,paste0(t)] <- table(panel.df[get(paste0("m21_",t))$obs_selection$obsRemoved,]$treatment)[2]/regyears
  # obs_table_m21[3,paste0(t)] <- table(panel.df[get(paste0("m21_",t))$obs_selection$obsRemoved,]$treatment)[1]/regyears

  # Fill observations table for all regressions. Comments show procedure for m12
  
  # m12  
  regyears <- length(unique(panel.df[get(paste0("m12_",t))$obs_selection$obsRemoved,]$year))
  # regyears extracts the number of years that enter the regression for the respective model specification
  
  obs_table_m12[1,paste0(t)] <- nrow(panel.df[get(paste0("m12_",t))$obs_selection$obsRemoved,])/regyears
  obs_table_m12[2,paste0(t)] <- table(panel.df[get(paste0("m12_",t))$obs_selection$obsRemoved,]$treatment)[2]/regyears
  obs_table_m12[3,paste0(t)] <- table(panel.df[get(paste0("m12_",t))$obs_selection$obsRemoved,]$treatment)[1]/regyears
  # object m12_t is the regression model. Obs_selection$obsRemoved gives a vector of observation IDs that were removed in the regression. 
  # The IDs in the vector have a negative sign -> code above selects only observations that entered the regression
  # nrow gives the total number of obs in the regression
  # number of treatment and control cells are extracted using table() 
  
  # m22
  regyears <- length(unique(panel.df[get(paste0("m22_",t))$obs_selection$obsRemoved,]$year))
  
  obs_table_m22[1,paste0(t)] <- nrow(panel.df[get(paste0("m22_",t))$obs_selection$obsRemoved,])/regyears
  obs_table_m22[2,paste0(t)] <- table(panel.df[get(paste0("m22_",t))$obs_selection$obsRemoved,]$treatment)[2]/regyears
  obs_table_m22[3,paste0(t)] <- table(panel.df[get(paste0("m22_",t))$obs_selection$obsRemoved,]$treatment)[1]/regyears
  
  # m32
  regyears <- length(unique(panel.df[get(paste0("m32_",t))$obs_selection$obsRemoved,]$year))
  
  obs_table_m32[1,paste0(t)] <- nrow(panel.df[get(paste0("m32_",t))$obs_selection$obsRemoved,])/regyears
  obs_table_m32[2,paste0(t)] <- table(panel.df[get(paste0("m32_",t))$obs_selection$obsRemoved,]$treatment)[2]/regyears
  obs_table_m32[3,paste0(t)] <- table(panel.df[get(paste0("m32_",t))$obs_selection$obsRemoved,]$treatment)[1]/regyears
  
}
m_list1_compare <- list(m11_2004, m12_2004, m11_2005, m12_2005, m11_2006, m12_2006, m11_2007, m12_2007, m11_2008, m12_2008, m11_2009, m12_2009, m11_2010, m12_2010, m11_2011, m12_2011, m11_2011, m12_2011, m11_2012, m12_2012, m11_2014, m12_2014, m11_2015, m12_2015, m11_2016, m12_2016, m11_2017, m12_2017, m11_2019, m12_2019)
m_list2_compare <- list(m21_2004, m22_2004, m21_2005, m22_2005, m21_2006, m22_2006, m21_2007, m22_2007, m21_2008, m22_2008, m21_2009, m22_2009, m21_2010, m22_2010, m21_2011, m22_2011, m21_2011, m22_2011, m21_2012, m22_2012, m21_2014, m22_2014, m21_2015, m22_2015, m21_2016, m22_2016, m21_2017, m22_2017, m21_2019, m22_2019)

m_list1 <- list(m12_2004, m12_2005, m12_2006, m12_2007, m12_2008, m12_2009, m12_2010, m12_2011, m12_2011, m12_2012, m12_2014, m12_2015, m12_2016, m12_2017, m12_2019)
m_list2 <- list(m22_2004, m22_2005, m22_2006, m22_2007, m22_2008, m22_2009, m22_2010, m22_2011, m22_2011, m22_2012, m22_2014, m22_2015, m22_2016, m22_2017, m22_2019)
m_list3 <- list(m32_2004, m32_2005, m32_2006, m32_2007, m32_2008, m32_2009, m32_2010, m32_2011, m32_2011, m32_2012, m32_2014, m32_2015, m32_2016, m32_2017, m32_2019)


names(m_list1_compare) <- sort(c(all_years, all_years))
names(m_list2_compare) <- sort(c(all_years, all_years))

names(m_list1) <- all_years
names(m_list2) <- all_years
names(m_list3) <- all_years

# Output: fc_loss
modelsummary(m_list1,
             output = "/datadrive/datalake/mapme.protectedareas/output/tabular/regression_output/table1_cluster_test.html",
             coef_rename = c("fc_loss" = "Forest cover loss", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Dependent variable: Forest cover loss"),
             add_rows = as.data.frame(obs_table_m12)
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
             output = "/datadrive/datalake/mapme.protectedareas/output/tabular/regression_output/table2_cluster_test.html",
             coef_rename = c("fc_area" = "Forest cover area", 
                             "treatment_disbTRUE" = "KfW support"),
             stars = TRUE,
             gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: .assetid", "FE: year"),
             title = paste0("Dependent variable: Forest cover area"),
             add_rows = as.data.frame(obs_table_m22)
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
             title = paste0("Dependent variable: Forest cover area"),
             add_rows = as.data.frame(obs_table_m32)
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





