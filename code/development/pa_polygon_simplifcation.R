# simplifcation of protected areas databases for mapdisplay via leaflet or speedup of spatial operations
# author: Johannes Schielein
library("rmapshaper")
library("sf")
library("mapview")

pas.supported <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg"
  )

pas.all <-
  st_read(
    "../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_valid.gpkg"
  )

pas.supported.simplified<-
  ms_simplify(pas.supported)

# fix broken geometries
pas.supported.simplified<-
  st_make_valid(pas.supported.simplified)

# pas.supported.simplified<-
#   st_buffer(pas.supported.simplified,dist = 0.000001)


print(object.size(pas.supported),units="Mb")
print(object.size(pas.supported.simplified),units="Mb")
mapview(pas.supported) + mapview(pas.supported.simplified)

# default seems acceptable as is

# simplify the rest
pas.all.simplified<-
  ms_simplify(pas.all)

# fix geometries
pas.all.simplified <-
  st_make_valid(pas.all.simplified)

pas.all.simplified <-
  st_make_valid(pas.all.simplified)

# pas.all.simplified <-
#   st_buffer(pas.all.simplified, dist = 0.000001)


# export the data
st_write(
  pas.supported.simplified,
  "../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique_simplified.gpkg",
  delete_layer=T
)

st_write(
  pas.all.simplified,
  "../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-04-22_allPAs_valid_simplified.gpkg",
  delete_layer=T
)
