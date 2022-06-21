# Author: Johannes Schielein
# Purpose: Move old database files
# last change: 2022-05-31


files<-
  list.files("../../datalake/mapme.protectedareas/output/matching/matching_frames/",full.names = T)

# create new dir
newdir<-paste("../../datalake/mapme.protectedareas/output/matching/matching_frames/arquived_", Sys.Date(), sep="")
dir.create(newdir)

# copy all files
lapply(files, file.copy, to = newdir)

# check if files where successfully moved
list.files(newdir)

# delete the old files
# lapply(files, file.remove, recursive=T)


