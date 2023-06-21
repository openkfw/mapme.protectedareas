# clean workspace, set options
rm(list=ls())
options(scipen=999)

# get packages
lop <- c("dplyr", "stargazer", "tidyverse")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)




#Load wdpa bmz keys
keys_wdpaid_bmz <- read_csv("~/shared/datalake/mapme.protectedareas/output/matching/model_frames/keys_wdpaid_bmz.csv") %>%
  rename("bmz_nummer" = "value")


bmz.df <- keys_wdpaid_bmz %>%                               # Summary by group using dplyr
  group_by(WDPAID) %>%
  summarize(bmz = toString(unique(bmz_nummer))
  )

# count number of bmz projects for each WDPA (important: command is "lenghts" and not "length")
bmz.df$bmz_count <- lengths(regmatches(bmz.df$bmz, gregexpr(",", bmz.df$bmz))) + 1

# tabulate distrubution of number of bmz projects per WDPA
table(bmz.df$bmz_count)

#  1   2   3 
#350  36   2 
