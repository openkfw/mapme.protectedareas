library(tidyverse)
library(fixest)

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

# Regression params
formula <- fc_area ~ treatment_disb | .assetid + year
my_pid <- ~.assetid+year
my_clust <- ~.assetid # what level to cluster on? 

# Map function takes a list/vector (bmzpa_filtered) as input and applies function (reg_area) to each object of the list 
# and returns a list
lregs <- map(bmzpa_filtered, function(x) reg_area(x, formula, my_pid, my_clust))

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
  labs(title = "Treatment effect estimates on Forest Cover Area")

# save forest plot
ggsave("/datadrive/yota/newfiles/plots/reg_fp_01_main.png",
       width = 8, height = 6)


