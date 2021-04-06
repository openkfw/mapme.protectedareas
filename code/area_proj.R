# area_proj.R
# Authors: Johannes Schielein, Om Prakash Bhandari
# Purpose: This script contains function which gives projstring based on the bounding box of the (sf) object that is supplie. 
# Note: The function relies heavily on code published at 'https://gis.stackexchange.com/questions/313721/automatically-get-an-adequate-projection-system-based-on-a-bounding-box/313725'


# area projection (Lambert Azimuthal Equal Area) with projection parameters 
# derived from feature extent

area_proj <- function(x) {
  bb <- sf::st_bbox(x)
  cntr_long <- (bb[3] - bb[1]) * 0.5 + bb[1]
  cntr_lat <- (bb[4] - bb[2]) * 0.5 + bb[2]
  bbm <- bb * 111000
  rng_x <- round(bbm[3] - bbm[1])
  rng_y <- round(bbm[4] - bbm[2])
  paste0("+proj=laea +lat_0=",
         cntr_lat,
         " +lon_0=",
         cntr_long,
         " +x_0=",
         rng_x,
         " +y_0=", 
         rng_y,
         " +a=6371007.181 +b=6371007.181 +units=m +no_defs")
}
