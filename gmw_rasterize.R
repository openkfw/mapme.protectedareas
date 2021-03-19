library(rgdal)
library(raster)
library(devtools)
install_github("s5joschi/gdalUtils")

library(gdalUtils)
setwd("6_kfw_globalmangrovewatch/")
mangroves.shp<-readOGR("/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/input/GMWDatenclipped/","GMW_clipped_2010")

# ----- for the clip region -----
gdal_rasterize("/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/input/GMWDatenclipped/GMW_clipped_2010.shp",
               "/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/output/GMW_clipped_2010AABB.tif",
               init=0,
               tr=c(1/111128*30,1/111128*30),
               ot="Byte",
               a="pxlval",
               co = c("COMPRESS=LZW","TFW=YES"),
               config = "GDAL_CACHEMAX 70%")

gdal_rasterize("/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/input/GMWDatenclipped/GMW_clipped_2016.shp",
               "/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/output/GMW_clipped_2016X.tif",
               init=0,
               te=paste(extent(mangroves.shp)[c(1,3,2,4)],collapse=" "),
               tr=c(1/111128*30,1/111128*30),
               ot="Byte",
               a="pxlval",
               config = "GDAL_CACHEMAX 70%")




r.gmw.2010<-raster("/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/output/GMW_clipped_2010.tif")
r.gmw.2016<-raster("/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/output/GMW_clipped_2016.tif")

r.gmw.dif<-r.gmw.2016-r.gmw.2010

# stack
rasters.stack<-stack(r.gmw.2010,r.gmw.2016,r.gmw.dif)

writeRaster(rasters.stack,
            "/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/output/mangrove-stack-clip.tif",
            dataType="INT2S")

# ----- for the wholetregion -----
gdal_rasterize("/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/input/OriginaldatenGMW/GMW_2010_v2.shp",
               "/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/output/GMW_2010_v2.tif",
               init=1,
               te=paste(extent(mangroves.shp)[c(1,3,2,4)],collapse=" "),
               tr=c(1/111128*30,1/111128*30),
               ot="Byte",
               a="pxlval")

gdal_rasterize("/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/input/OriginaldatenGMW/GMW_2016_v2.shp",
               "/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/output/GMW_2016_v2.tif",
               init=1,
               te=paste(extent(mangroves.shp)[c(1,3,2,4)],collapse=" "),
               tr=c(1/111128*30,1/111128*30),
               ot="Byte",
               a="pxlval")

r.gmw.2010<-raster("/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/output/GMW_2010_v2.tif")
r.gmw.2016<-raster("/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/output/GMW_2016_v2.tif")

r.gmw.dif<-r.gmw.2016-r.gmw.2010

# stack
rasters.stack<-stack(r.gmw.2010,r.gmw.2016,r.gmw.dif)

writeRaster(rasters.stack,
            "/home/jschielein/R/R-projects/6_kfw_globalmangrovewatch/output/mangrove-stack-global.tif",
            dataType="INT1S")



