# dopa-rest.R
# Authors: Om Bhandari, Johannes Schielein
# Purpose: This script contains functions to create Queries to the DOPA Rest Services from JRC
# Note: Please make sure you are registered with DOPA and have the required authorization for using REST Services (non commercial usage only)


# (1) get_redlist_status
# request redlist_status per wdpaid
# Returns statistics (counts species, by class, by IUCN categories) for species (Corals, Sharks & Rays, Amphibians, Birds, Mammals) in Protected Area;...
# ...calculated as intersection of species ranges with WDPA

get_redlist_status <- function(wdpaid) {
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
}

# example
# get_redlist_status(146)



# (2) get_species_list
# request redlist_list per wdpaid
# Returns list of species (Corals, Sharks & Rays, Amphibians, Birds, Mammals) in Protected Area; calculated as intersection of species ranges with WDPA

get_species_list <- function(wdpaid) {
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
}

# example
# get_species_list(63645)



# (3) get_wdpa_level_centroid
# request get_wdpa_level_centroid per wdpaid
# Calculates WDPA point coordinates (x,y) in EPSG 4326 (Lat Long WGS84) for points PAs real coords are shown. For polygon PAs centroids are calculated with the...
# ...function ST_PointOnSurface, which returns a point guaranteed to lie on the surface.

get_wdpa_level_centroid <- function(wdpaid) {
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
}

# example
# get_wdpa_level_centroid(555528898)



# (4) get_water_stats
# request get_pa_water-stats per wdpaid
# Returns information on the current surface area of permanent and seasonal water, and the net change over the period 1984-2015

get_water_stats <- function(wdpaid) {
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
}

# example
# get_water_stats(671)



# (5) get_landcover_esa
# request get_wdpa_lc-esa per wdpaid and year(1995,2000,2005,2010,2015) & aggregation level (0,1,2,3)
# Returns percentage and absolute cover of different ESA CCI LC classes for a given WDPA Aggregation levels 0 (original ESA LC classes), 1, 2 and 3 are available.

get_landcover_esa <- function(wdpaid, year, agg) {
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
}

# example
# get_landcover_esa(32671, 2015, 0)



# (6) get_landcover_copernicus
# request get_wdpa_lc_copernicus per wdpaid and year(2015) & aggregation level (0,2)
# Returns percentage and absolute cover of Copernicus Land Cover classes for a given WDPA...
#...Aggregation levels 0 (original Copernicus LC classes) and 2 (DOPA) are available.

get_landcover_copernicus <- function(wdpaid, agg) {
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
}

# example
# get_landcover_copernicus(32671, 2)



# (7) get_lcc_esa
# request get_wdpa_lcc-esa per wdpaid
# For a given WDPA, returns absolute cover of ESA LC CCI classes (aggregated by level 1: 4 classes) which changed within first and last epoch i.e. 1995 & 2015

get_lcc_esa <- function(wdpaid) {
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
}

# example
# get_lcc_esa(32671)



# (8) get_lcc_esa_percent
# request get_wdpa_lcc_esa_percent per wdpaid
# For a given WDPA, returns percentage and absolute cover of ESA LC CCI classes which changed within first and last epoch.

get_lcc_esa_percent <- function(wdpaid) {
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
}

# example
# get_lcc_esa_percent(32671)



# (9) get_all_indicators
# request get_wdpa_all_indicators per wdpaid
# Returns all indicators for pa

get_all_indicators <- function(wdpaid) {
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
}

# example
# get_all_indicators(142)



## (10) get_country_pa_normalized_ind
# request get_country_pa_normalized_indicator per indicator listed in "all_indicators"
# Returns, for protected area in country, absolute, normalized and average value of the...
# ...selected indicator, and ranking within the country

get_country_pa_normalized_ind <- function(ind) {
  # create the url
  url <- paste0("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/administrative_units/get_country_pa_normalized_indicator?format=csv&indicator=",ind,"&iso=ITA")
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
  destfile <- paste0(tempdir(),"/country_pa_normalized_indicator_",ind,".csv")
  # download the file
  download.file(url, destfile)
  # read in temporary csv
  tmp.df <- read.csv(paste0(tempdir(),"/country_pa_normalized_indicator_",ind,".csv"),sep="|")
  # delete the temporary file
  file.remove(paste0(tempdir(),"/country_pa_normalized_indicator_",ind,".csv"))
  # append the results of the file to an existing dataframe/table and return results
  return(rbind(df.normalizedind_country,tmp.df)[-1,])
}

# example
# get_country_pa_normalized_ind("agri_ind_pa")



# (11) get_ecoregion_pa_normalized_ind
# request get_ecoregion_pa_normalized_indicator per indicator listed in "all_indicators"
# Returns, for protected area in ecoregion, absolute, normalized and average value of the selected indicator, and ranking within the ecoregion

get_ecoregion_pa_normalized_ind <- function(ind, id) {
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
}

# example
# get_ecoregion_pa_normalized_ind("agri_ind_pa", 81214)



# (12) get_dopa
# -------------
# Argument 'Topic': "protected_sites", "species", "water", "landcover"
# -------------
# Argument 'getQuery': "get_wdpa_level_centroid", "get_pa_water_stats", "get_pa_redlist_status", "get_pa_redlist_list"...
# ..."get_wdpa_all_inds", "get_wdpa_lcc_esa" & "get_wdpa_lcc_esa_percent"
# ------------
# for other two services "get_wdpa_lc_esa" & "get_wdpa_lc_copernicus" -- reuires two more arguments i.e...
# .. "agg" and "year" -- so global function cannot be applied. Simply opt for individual functions

get_dopa <- function(topic, getQuery, wdpaid) {
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
}

# examples
# get_dopa("species", "get_pa_redlist_status", 146)
# get_dopa("landcover", "get_wdpa_lcc_esa", 32671)
# get_dopa("protected_sites", "get_wdpa_all_inds", 142)
