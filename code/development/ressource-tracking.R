install.packages("NCmisc")
library("NCmisc")
library("tidyverse")

# create a list for one hour of monitoring
ressource.table<-as.list(1:(60*60*1))

# create monitoring observations for each second. 
for (i in 1:length(ressource.table)){
  ressource.table[[i]]<-top(Table=TRUE,procs=100)
  ressource.table[[i]]$Table$SYS.TIME<-Sys.time()
  Sys.sleep(1) # sleep one sec 
}

# rbind all the information
ressource.table.complete<-ressource.table[[1]]$Table

for (i in 2:length(ressource.table)){
  ressource.table.complete<-rbind(ressource.table.complete,ressource.table[[i]]$Table)
}

nrow(ressource.table.complete)

# convert values
ressource.table.complete$CPU<-as.numeric(ressource.table.complete$`%CPU`)
ressource.table.complete$MEM<-as.numeric(ressource.table.complete$`%MEM`)

# aggregate by user and calculate mean usage over all parallelized processes. 
ressource.table.complete.agg <-
  ressource.table.complete %>%
  filter(USER == "ombhand+")%>%
  group_by(SYS.TIME) %>%
  summarise(CPU_mean = mean(CPU),MEM_mean=mean(MEM))

# plot mean usage fo rthe first ten minutes or so
ggplot(ressource.table.complete.agg[1:500,])+
  geom_line(aes(SYS.TIME,CPU_mean),color="blue")+
  geom_line(aes(SYS.TIME,MEM_mean),color="red")+
  ylab("% CPU usage (blue) and memory usage (red)")
