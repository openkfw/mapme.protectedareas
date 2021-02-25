#  dopa-rest.R
download.file("https://dopa-services.jrc.ec.europa.eu/services/d6dopa40/protected_sites/get_wdpa_level_centroid?format=csv&wdpaid=555528898",
              destfile = "1_pre-processing/dopa-rest/test.csv")

test<-read.csv("1_pre-processing/dopa-rest/test.csv",sep="|")
View(test)
