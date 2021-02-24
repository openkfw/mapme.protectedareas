library(leaflet)

set.seed(12345)
df <- data.frame(lat=runif(30, min=50, max=56), long=runif(30, min=6, max=10))
head(df)

# this looks nicer