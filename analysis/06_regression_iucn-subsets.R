library(tidyverse)
library(fixest)
library(readxl)

# -------------- Preparation

# list of panel datasets
files <- list.files("/datadrive/datalake/mapme.protectedareas/output/tabular/regression_input/CEM/tn_project_pa") %>% 
  grep("cem_tn_matched_panel_*", ., value = TRUE)
bmzpa_list <- files %>% 
  gsub("cem_tn_matched_panel_", "", .) %>% 
  gsub(".csv", "", .)

# List of BMZ-PA that we want (follow-up phase problem)
bmzwdpa_nofu <- read_csv("/datadrive/yota/newfiles/data/bmzwdpa_nofu.csv") %>% 
  unite("bmzpa", c(bmz, wdpaid))
relevant <- bmzpa_list %in% bmzwdpa_nofu$bmzpa
bmzpa_filtered <- bmzpa_list[relevant]

# Load IUCN action types table
iucnat <- read_excel("/datadrive/yota/newfiles/data/iucnat.xlsx",
                   sheet = "classification") %>%
  unite("bmzpa", c(bmz, wdpaid))

iucn_nrs <- iucnat %>% 
  select(starts_with("iucn")) %>%
  names()


# -------------- Functions

# Function for getting bmzpa list subsets, function returns a vector of bmzpa 
get_iucnat_subsets <- function(iucn_nr) {
  
  # Get PAs that have a focus on respective iucn action type
  mydat <- iucnat %>%
    filter(get(iucn_nr) == 2) %>%
    pull(bmzpa)
  
  relevant <- bmzpa_filtered %in% mydat
  bmzpa_filtered[relevant]
}

############## Follow-up problem in iucn cat case!

# Function for regression
reg_area <- function(bmzpa, formula, my_pid, my_clust) {
  
  # Load panel as regression input
  mydat <- read_csv(paste0("/datadrive/datalake/mapme.protectedareas/output/tabular/regression_input/CEM/tn_project_pa/cem_tn_matched_panel_",
                           bmzpa, ".csv"))
  
  # Give a name to regression object 
  nam <- "reg"
  
  # Regression estimation, taking function inputs as regression parameters
  model <- feols(formula, data = mydat, weights = ~weights_cem, 
                 panel.id = my_pid, cluster = my_clust)
  
  # Summary as output of function
  summary(model)
}

# Function that creates the forest plot based on a vector of bmzpa numbers
forest_plot <- function(bmzpalist, iucn_name, formula, my_pid, my_clust) {
  
  if (length(bmzpalist) == 0) {
    return(paste0("No PAs with major focus on ", iucn_name,"."))
  } else {
    # Map function takes a list/vector (bmzpa_filtered) as input and applies function (reg_area) to each object of the list 
    # and returns a list
    lregs <- map(bmzpalist, function(x) reg_area(x, formula, my_pid, my_clust))
    
    # Sort by coefficient size for forest plot ordering
    lregs_sorted <- lregs[order(sapply(lregs, function(x) x$coefficients), decreasing = TRUE)]
    
    # Extract coefs and ses; needed for plotting
    coefs <- sapply(lregs_sorted, function(x) x$coefficients)
    sds <- sapply(lregs_sorted, function(x) x$se)
    
    # Create a tibble with coefs and ses as data
    coef_df <- tibble(coefs, sds)
    coef_df$id <- 1:nrow(coef_df)
    
    # ggplot for forest plot
    ggplot(coef_df, aes(id, coefs)) + 
      geom_hline(yintercept = 0, lty = 2, lwd = 0.5, colour = "grey50") +
      geom_errorbar(aes(ymin = coefs - 1.96  * sds, 
                        ymax = coefs + 1.96 * sds), 
                    lwd = 0.2, col = "darkred", width = 0) +
      geom_point(size = 0.45, pch = 21, fill = "darkblue", col = "darkblue") +
      xlab(NULL) +
      coord_flip() +
      theme_bw() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid = element_blank()) +
      ylab("Estimated effect size") +
      labs(title = paste0("Forest cover area: major focus on ", iucn_name))
    
    # save forest plot
    ggsave(paste0("/datadrive/yota/newfiles/plots/reg_fp_02_", iucn_name, ".png"),
           width = 8, height = 6) 
  }
}


# -------------- Analysis


# Get a list of vectors with relevant bmzpa subsets
iucn_subsets <- map(iucn_nrs, function(x) get_iucnat_subsets(x))

# Regression params
formula <- fc_area ~ treatment_disb | .assetid + year
my_pid <- ~.assetid+year
my_clust <- ~.assetid # what level to cluster on? 

# Create forest plots for all subsets
map2(iucn_subsets, iucn_nrs, function(x, y) forest_plot(bmzpalist = x, iucn_name = y, formula, my_pid, my_clust))
