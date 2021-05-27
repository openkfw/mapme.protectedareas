# dopa-rest.R
# Authors: Om Bhandari, Johannes Schielein
# Purpose: This script contains functions to create Queries to the DOPA Rest Services from JRC
# Note: Please make sure you are registered with DOPA and have the required authorization for using REST Services (non commercial usage only)

# load latest PA polygon -------------------------------------------------------
p <- read_sf("/home/rstudio/shared/datalake/mapme.protectedareas/output/polygon/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs.gpkg")

# Introduction to functions ----------------------------------------------------

# There are total 12 functions available in this script. Out of these 12 functions, 7 of them takes only `WDPAID` as argument.

# Those functions are ordered from 1 to 7 here in this script and they are:
# (1) get_redlist_status
# (2) get_species_list
# (3) get_wdpa_level_centroid
# (4) get_water_stats
# (5) get_lcc_esa
# (6) get_lcc_esa_percent
# (7) get_multiple_indicators

# From the `get_multiple_indicators`, we get the data access to 160 different variables including water stats, temperature, precipitation, mspa, elevation,.....
# ....global forest cover, population count & density, livestock, above ground biomass, below ground biomass, global soil organic carbon, etc.

# However there is another function which can access the results from all above mentioned 7 functions i.e. `get_dopa`
# (8) get_dopa

# As we can see here, the more powerful function `get_dopa` can be used instead of using the above listed 7 functions. 
# If we need to analyze only one type of data then using particular function will be useful since we can get the output results in ready to use dataframe format.
# Using `get_dopa` however demands post tidying of data as per requirement.

# Remaining functions which can not be accessed using above mentioned functions and requires multiple arguments are:
# (9) get_landcover_esa
# (10) get_landcover_copernicus
# (11) get_country_pa_normalized_ind
# (12) get_ecoregion_pa_normalized_ind


# (1) get_redlist_status -------------------------------------------------------

# request redlist_status per wdpaid
# Returns statistics (counts species, by class, by IUCN categories) for species (Corals, Sharks & Rays, Amphibians, Birds, Mammals) in Protected Area;...
# ...calculated as intersection of species ranges with WDPA

get_redlist_status <- function(wdpaid) {
  tryCatch(
    {
  # create the url
  url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/species/get_pa_redlist_status?format=csv&wdpaid=",wdpaid)
  # create empty dataframe to receive results
  df.redlist_status<-data.frame(class=NA,
                                total_species=NA,
                                threatened=NA,
                                critically_endangered=NA,
                                endangered=NA,
                                vulnerable=NA,
                                near_threatened=NA,
                                least_concern=NA,
                                data_deficient=NA,
                                wdpa_id=NA)
  # create string for temporary file - string should be in temp folder
  destfile <- paste0(tempdir(),"/redlist_status_",wdpaid,".csv")
  # download the file
  download.file(url, destfile)
  # read in temporary csv
  tmp.df <- read.csv(paste0(tempdir(),"/redlist_status_",wdpaid,".csv"),sep="|")
  # create a column from wdpa id
  tmp.df$wdpa_id <- wdpaid
  # delete the temporary file
  file.remove(paste0(tempdir(),"/redlist_status_",wdpaid,".csv"))
  # append the results of the file to an existing dataframe/table and return results
  return(rbind(df.redlist_status,tmp.df)[-1,])
    },
  error = function(e) {
    message('Error in this line!')
  }
  )
}

# # example
# df_redlist_status <- get_redlist_status(p[1, ]$WDPAID)
# 
# # process other polygons
# for (i in 2:nrow(p)) {
#   
#   df_redlist_status <- 
#     rbind(df_redlist_status,
#           get_redlist_status(p[i, ]$WDPAID))
#   print(paste("Done processing line", i, sep=" "))
# }
# 
# write.csv(df_redlist_status, file = "/home/rstudio/shared/datalake/mapme.protectedareas/output/polygon/dopa-rest/redlist_status.csv", row.names = F)


# (2) get_species_list ---------------------------------------------------------

# request redlist_list per wdpaid
# Returns list of species (Corals, Sharks & Rays, Amphibians, Birds, Mammals) in Protected Area; calculated as intersection of species ranges with WDPA

get_species_list <- function(wdpaid) {
  
  tryCatch(
    {
  # create the url
  url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/species/get_pa_redlist_list?format=csv&wdpaid=",wdpaid)
  # create empty dataframe to receive results
  df.redlist_list<-data.frame(iucn_species_id=NA,
                              taxon=NA,
                              kingdom=NA,
                              phylum=NA,
                              class=NA,
                              order=NA,
                              family=NA,
                              code=NA,
                              wdpa_id=NA)
  # create string for temporary file - PLEASE ADAPT TO TEMPDIR - SEE ABOVE
  destfile <- paste0(tempdir(),"/redlist_list_",wdpaid,".csv")
  # download the file
  download.file(url, destfile)
  # read in temporary csv
  tmp.df <- read.csv(paste0(tempdir(),"/redlist_list_",wdpaid,".csv"),sep="|")
  # create a column from wdpa id
  tmp.df$wdpa_id <- wdpaid
  # rename the columns to match the example dataset
  colnames(tmp.df) <- colnames(df.redlist_list)
  # delete the temporary file
  file.remove(paste0(tempdir(),"/redlist_list_",wdpaid,".csv"))
  # append the results of the file to an existing dataframe/table and return results
  return(rbind(df.redlist_list,tmp.df)[-1,])
    },
  error = function(e) {
    message('Error in this line!')
  }
  )
}

# # example
# df_species_list <- get_species_list(p[1, ]$WDPAID)
# 
# # process other polygons
# for (i in 2:nrow(p)) {
#   
#   df_species_list <- 
#     rbind(df_species_list,
#           get_species_list(p[i, ]$WDPAID))
#   print(paste("Done processing line", i, sep=" "))
# }
# 
# write.csv(df_species_list, file = "/home/rstudio/shared/datalake/mapme.protectedareas/output/polygon/dopa-rest/species_list.csv", row.names = F)



# (3) get_wdpa_level_centroid --------------------------------------------------

# request get_wdpa_level_centroid per wdpaid
# Calculates WDPA point coordinates (x,y) in EPSG 4326 (Lat Long WGS84) for points PAs real coords are shown. For polygon PAs centroids are calculated with the...
# ...function ST_PointOnSurface, which returns a point guaranteed to lie on the surface.

get_wdpa_level_centroid <- function(wdpaid) {
  
  tryCatch(
    {
  # create the url
  url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/protected_sites/get_wdpa_level_centroid?format=csv&wdpaid=",wdpaid)
  # create empty dataframe to receive results
  df.wdpa_level_centroid <- data.frame(wdpaid=NA,
                                       wdpa_pid=NA,
                                       name=NA,
                                       iso3=NA,
                                       x=NA,
                                       y=NA)
  # create string for temporary file
  destfile <- paste0(tempdir(),"/wdpa_level_centroid_",wdpaid,".csv")
  # download the file
  download.file(url, destfile)
  # read in temporary csv
  tmp.df <- read.csv(paste0(tempdir(),"/wdpa_level_centroid_",wdpaid,".csv"),sep="|")
  # delete the temporary file
  file.remove(paste0(tempdir(),"/wdpa_level_centroid_",wdpaid,".csv"))
  # append the results of the file to an existing dataframe/table and return results
  return(rbind(df.wdpa_level_centroid,tmp.df)[-1,])
    },
  error = function(e) {
    message('Error in this line!')
  }
  )
}

# # example
# df_centroid <- get_wdpa_level_centroid(p[1, ]$WDPAID)
# 
# # process other polygons
# for (i in 2:nrow(p)) {
#   
#   df_centroid <- 
#     rbind(df_centroid,
#           get_wdpa_level_centroid(p[i, ]$WDPAID))
#   print(paste("Done processing line", i, sep=" "))
# }
# 
# write.csv(df_centroid, file = "/home/rstudio/shared/datalake/mapme.protectedareas/output/polygon/dopa-rest/wdpa_level_centroid.csv", row.names = F)



# (4) get_water_stats ----------------------------------------------------------

# request get_pa_water-stats per wdpaid
# Returns information on the current surface area of permanent and seasonal water, and the net change over the period 1984-2015

get_water_stats <- function(wdpaid) {
  
  tryCatch(
    {
      
      # create the url
      url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/water/get_pa_water_stats?format=csv&wdpa_id=",wdpaid)
      # create empty dataframe to receive results
      df.water_stats <- data.frame(wdpaid=NA,
                                   perm_now_km2=NA,
                                   seas_now_km2=NA,
                                   net_p_change_km2=NA,
                                   net_s_change_km2=NA,
                                   percent_net_perm_change=NA,
                                   percent_net_seas_change=NA)
      # create string for temporary file
      destfile <- paste0(tempdir(),"/water_stats_",wdpaid,".csv")
      # download the file
      download.file(url, destfile)
      # read in temporary csv
      tmp.df <- read.csv(paste0(tempdir(),"/water_stats_",wdpaid,".csv"),sep="|")
      # delete the temporary file
      file.remove(paste0(tempdir(),"/water_stats_",wdpaid,".csv"))
      # append the results of the file to an existing dataframe/table and return results
      return(rbind(df.water_stats,tmp.df)[-1,])
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# # example
# df_water <- get_water_stats(p[1, ]$WDPAID)
# 
# # process other polygons
# for (i in 2:nrow(p)) {
#   
#   df_water <- 
#     rbind(df_water,
#           get_water_stats(p[i, ]$WDPAID))
#   print(paste("Done processing line", i, sep=" "))
# }
# 
# write.csv(df_water, file = "/home/rstudio/shared/datalake/mapme.protectedareas/output/polygon/dopa-rest/water_stats.csv", row.names = F)



# (5) get_lcc_esa --------------------------------------------------------------

# request get_wdpa_lcc-esa per wdpaid
# For a given WDPA, returns absolute cover of ESA LC CCI classes (aggregated by level 1: 4 classes) which changed within first and last epoch i.e. 1995 & 2015

get_lcc_esa <- function(wdpaid) {
  
  tryCatch(
    {
      # create the url
      url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/landcover/get_wdpa_lcc_esa?format=csv&wdpaid=",wdpaid)
      # create empty dataframe to receive results
      df.landcover_change <- data.frame(lc1_1995=NA,
                                        lc2_2015=NA,
                                        area=NA,
                                        wdpa_id=NA)
      # create string for temporary file
      destfile <- paste0(tempdir(),"/lcc_esa",wdpaid,".csv")
      # download the file
      download.file(url, destfile)
      # read in temporary csv
      tmp.df <- read.csv(paste0(tempdir(),"/lcc_esa",wdpaid,".csv"),sep="|")
      # create a column from wdpa id
      tmp.df$wdpa_id <- wdpaid
      # delete the temporary file
      file.remove(paste0(tempdir(),"/lcc_esa",wdpaid,".csv"))
      # append the results of the file to an existing dataframe/table and return results
      return(rbind(df.landcover_change,tmp.df)[-1,])
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# # example
# df_lcc_esa <- get_lcc_esa(p[1, ]$WDPAID)
# 
# # process other polygons
# for (i in 2:nrow(p)) {
#   
#   df_lcc_esa <- 
#     rbind(df_lcc_esa,
#           get_lcc_esa(p[i, ]$WDPAID))
#   print(paste("Done processing line", i, sep=" "))
# }
# 
# write.csv(df_lcc_esa, file = "/home/rstudio/shared/datalake/mapme.protectedareas/output/polygon/dopa-rest/landcover_change_esa.csv", row.names = F)



# (6) get_lcc_esa_percent ------------------------------------------------------

# request get_wdpa_lcc_esa_percent per wdpaid
# For a given WDPA, returns percentage and absolute cover of ESA LC CCI classes which changed within first and last epoch.

get_lcc_esa_percent <- function(wdpaid) {
  
  tryCatch(
    {
      # create the url
      url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/landcover/get_wdpa_lcc_esa_percent?format=csv&wdpaid=",wdpaid)
      # create empty dataframe to receive results
      df.lcc_percent <- data.frame(percent=NA,
                                   area_lcc=NA,
                                   area_pa=NA,
                                   wdpa_id=NA)
      # create string for temporary file
      destfile <- paste0(tempdir(),"/lcc_esa_percent_",wdpaid,".csv")
      # download the file
      download.file(url, destfile)
      # read in temporary csv
      tmp.df <- read.csv(paste0(tempdir(),"/lcc_esa_percent_",wdpaid,".csv"),sep="|")
      # create a column from wdpa id
      tmp.df$wdpa_id <- wdpaid
      # delete the temporary file
      file.remove(paste0(tempdir(),"/lcc_esa_percent_",wdpaid,".csv"))
      # append the results of the file to an existing dataframe/table and return results
      return(rbind(df.lcc_percent,tmp.df)[-1,])
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# example
# get_lcc_esa_percent(32671)

# (7) get_multiple_indicators --------------------------------------------------

# request get_wdpa_all_indicators per wdpaid
# Returns all indicators for pa

get_multiple_indicators <- function(wdpaid) {
  
  tryCatch(
    {
      # create the url
      url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/protected_sites/get_wdpa_all_inds?format=csv&wdpaid=",wdpaid)
      # create string for temporary file
      destfile <- paste0(tempdir(),"/all_indicators_",wdpaid,".csv")
      # download the file
      download.file(url, destfile)
      # read in temporary csv
      tmp.df <- read.csv(paste0(tempdir(),"/all_indicators_",wdpaid,".csv"),sep="|")
      # delete the temporary file
      file.remove(paste0(tempdir(),"/all_indicators_",wdpaid,".csv"))
      # append the results of the file to an existing dataframe/table and return results
      return(tmp.df)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# # example
# df_multiple_indicators <- get_multiple_indicators(p[1, ]$WDPAID)
# 
# # process other polygons
# for (i in 2:nrow(p)) {
#   
#   df_multiple_indicators <- 
#     rbind(df_multiple_indicators,
#           get_multiple_indicators(p[i, ]$WDPAID))
#   print(paste("Done processing line", i, sep=" "))
# }
# 
# write.csv(df_multiple_indicators, file = "/home/rstudio/shared/datalake/mapme.protectedareas/output/polygon/dopa-rest/multiple_indicators.csv", row.names = F)



# (8) get_dopa ----------------------------------------------------------------

# With `get_dopa` we can get the results for the seven different getQueries for which there are seven different functions created above
# Since, topic and getQuery should come in sequence. Here I have listed the getQueries and their respective topic to use.
# (a) species
#             - get_pa_redlist_status
#             - get_pa_redlist_list

# (b) water
#             - get_pa_water_stats

# (c) protected_sites
#             - get_wdpa_level_centroid
#             - get_wdpa_all_inds

# (d) landcover
#             - get_wdpa_lcc_esa
#             - get_wdpa_lcc_esa_percent


get_dopa <- function(topic, getQuery, wdpaid) {
  
  tryCatch(
    {
      # create the url
      url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/",topic,"/",getQuery,"?format=csv&wdpaid=",wdpaid)
      # create string for temporary file
      destfile <- paste0(tempdir(),"/",getQuery,"_",wdpaid,".csv")
      # download the file
      download.file(url, destfile)
      # read in temporary csv
      tmp.df <- read.csv(paste0(tempdir(),"/",getQuery,"_",wdpaid,".csv"),sep="|")
      # delete the temporary file
      file.remove(paste0(tempdir(),"/",getQuery,"_",wdpaid,".csv"))
      # append the results of the file to an existing dataframe/table and return results
      return(tmp.df)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# examples
# get_dopa("species", "get_pa_redlist_status", 146)
# get_dopa("landcover", "get_wdpa_lcc_esa", 32671)
# get_dopa("protected_sites", "get_wdpa_all_inds", 142)


# (9) get_landcover_esa --------------------------------------------------------

# request get_wdpa_lc-esa per wdpaid and year(1995,2000,2005,2010,2015) & aggregation level (0,1,2,3)
# Returns percentage and absolute cover of different ESA CCI LC classes for a given WDPA Aggregation levels 0 (original ESA LC classes), 1, 2 and 3 are available.

get_landcover_esa <- function(wdpaid, year, agg) {
  
  tryCatch(
    {
      # create the url
      url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/landcover/get_wdpa_lc_esa?format=csv&wdpaid=",wdpaid,"&year=",year,"&agg=",agg)
      # create empty dataframe to receive results
      df.landcover_esa <- data.frame(percent=NA,
                                     area=NA,
                                     lc_class=NA,
                                     label=NA,
                                     color=NA,
                                     wdpa_id=NA)
      # create string for temporary file
      destfile <- paste0(tempdir(),"/landcover_esa_",wdpaid,"_",year,"_",agg,".csv")
      # download the file
      download.file(url, destfile)
      # read in temporary csv
      tmp.df <- read.csv(paste0(tempdir(),"/landcover_esa_",wdpaid,"_",year,"_",agg,".csv"),sep="|")
      # create a column from wdpa id
      tmp.df$wdpa_id <- wdpaid
      # delete the temporary file
      file.remove(paste0(tempdir(),"/landcover_esa_",wdpaid,"_",year,"_",agg,".csv"))
      # append the results of the file to an existing dataframe/table and return results
      return(rbind(df.landcover_esa,tmp.df)[-1,])
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# example
# get_landcover_esa(32671, 2015, 0)



# (10) get_landcover_copernicus -------------------------------------------------

# request get_wdpa_lc_copernicus per wdpaid and year(2015) & aggregation level (0,2)
# Returns percentage and absolute cover of Copernicus Land Cover classes for a given WDPA...
#...Aggregation levels 0 (original Copernicus LC classes) and 2 (DOPA) are available.

get_landcover_copernicus <- function(wdpaid, agg) {
  
  tryCatch(
    {
      # create the url
      url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/landcover/get_wdpa_lc_copernicus?format=csv&agg=",agg,"&wdpaid=",wdpaid,"&year=2015")
      # create empty dataframe to receive results
      df.landcover_copernicus <- data.frame(percent=NA,
                                            area=NA,
                                            lc_class=NA,
                                            label=NA,
                                            color=NA,
                                            wdpa_id=NA)
      # create string for temporary file
      destfile <- paste0(tempdir(),"/landcover_copernicus_",wdpaid,"_",agg,".csv")
      # download the file
      download.file(url, destfile)
      # read in temporary csv
      tmp.df <- read.csv(paste0(tempdir(),"/landcover_copernicus_",wdpaid,"_",agg,".csv"),sep="|")
      # create a column from wdpa id
      tmp.df$wdpa_id <- wdpaid
      # delete the temporary file
      file.remove(paste0(tempdir(),"/landcover_copernicus_",wdpaid,"_",agg,".csv"))
      # append the results of the file to an existing dataframe/table and return results
      return(rbind(df.landcover_copernicus,tmp.df)[-1,])
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# # example
# df_copernicus <- get_landcover_copernicus(p[1, ]$WDPAID, 0)
# 
# # process other polygons
# for (i in 2:nrow(p)) {
#   
#   df_copernicus <- 
#     rbind(df_copernicus,
#           get_landcover_copernicus(p[i, ]$WDPAID, 0))
#   print(paste("Done processing line", i, sep=" "))
# }
# 
# write.csv(df_copernicus, file = "/home/rstudio/shared/datalake/mapme.protectedareas/output/polygon/dopa-rest/landcover_copernicus.csv", row.names = F)



# (11) get_country_pa_normalized_ind ------------------------------------------

# request get_country_pa_normalized_indicator per indicator listed in "multiple_indicators" and also takes ISO code of country
# Returns, for protected area in country, absolute, normalized and average value of the....
# ....selected indicator, and ranking within the country

get_country_pa_normalized_ind <- function(ind, iso) {
  
  tryCatch(
    {
      # create the url
      url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/administrative_units/get_country_pa_normalized_indicator?format=csv&indicator=",ind,"&iso=",iso)
      # create empty dataframe to receive results
      df.normalizedind_country <- data.frame(country_rank=NA,
                                             wdpaid=NA,
                                             iso3=NA,
                                             iso2=NA,
                                             un_m49=NA,
                                             ind=NA,
                                             normalized_ind=NA,
                                             normalized_avg_ind=NA)
      # create string for temporary file
      destfile <- paste0(tempdir(),"/country_pa_normalized_indicator_",ind,"_",iso,".csv")
      # download the file
      download.file(url, destfile)
      # read in temporary csv
      tmp.df <- read.csv(paste0(tempdir(),"/country_pa_normalized_indicator_",ind,"_",iso,".csv"),sep="|")
      # delete the temporary file
      file.remove(paste0(tempdir(),"/country_pa_normalized_indicator_",ind,"_",iso,".csv"))
      # append the results of the file to an existing dataframe/table and return results
      return(rbind(df.normalizedind_country,tmp.df)[-1,])
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# # example
# get_country_pa_normalized_ind("agri_ind_pa", "VEN")


# (12) get_ecoregion_pa_normalized_ind -----------------------------------------

# request get_ecoregion_pa_normalized_indicator per indicator listed in "multiple_indicators" and requires respective ecoregion id
# Returns, for protected area in ecoregion, absolute, normalized and average value of the selected indicator, and ranking within the ecoregion

get_ecoregion_pa_normalized_ind <- function(ind, id) {
  
  tryCatch(
    {
      # create the url
      url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/habitats_and_biotopes/get_ecoregion_pa_normalized_indicator?format=csv&indicator=",ind,"&ecoregionid=",id)
      # create empty dataframe to receive results
      df.normalizedind_ecoregion <- data.frame(ecoregion_rank=NA,
                                               wdpa_id=NA,
                                               ecoregion_id=NA,
                                               ind=NA,
                                               normalized_ind=NA,
                                               normalized_avg_ind=NA)
      # create string for temporary file
      destfile <- paste0(tempdir(),"/ecoregion_pa_normalized_indicator_",ind,"_",id,".csv")
      # download the file
      download.file(url, destfile)
      # read in temporary csv
      tmp.df <- read.csv(paste0(tempdir(),"/ecoregion_pa_normalized_indicator_",ind,"_",id,".csv"),sep="|")
      # delete the temporary file
      file.remove(paste0(tempdir(),"/ecoregion_pa_normalized_indicator_",ind,"_",id,".csv"))
      # append the results of the file to an existing dataframe/table and return results
      return(rbind(df.normalizedind_ecoregion,tmp.df)[-1,])
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}

# example
# get_ecoregion_pa_normalized_ind("agri_ind_pa", 81214)
