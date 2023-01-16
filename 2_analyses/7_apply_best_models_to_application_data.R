gdata::keep(dfs.fixed.0.4, dfs.flex.100, sure=T)
source('functions.R') # reload the functions

# load the gps and acc data, available at https://doi.org/10.25850/nioz/7b.b.yd. 
bird.data <- data.frame(logger=c(6283,6284,6285,6289,6291,6294,6284,6298),sex=rep("F",8),year.start=c(rep(2016,6),rep(2017,2)), year.end=c(2020,2017,2017,rep(2020,5))) 
gps.acc.data.list <- list()
for (i in 1:dim(bird.data)[1]) {
  filename <- paste("data/raw/gps.acc.data.", bird.data$logger[i],"_", bird.data$year.start[i],".csv", sep="")
  gps.acc.data.list[[i]] <- read.csv(filename)
}

# train best-supported RF models on all annotated data:
RF.fixed.0.4 <- RF.model.start(dfs.fixed.0.4[[1]], stand=4)
RF.flexible.100 <- RF.model.start(dfs.flex.100[[1]], stand=4)

# do the segmentation and classification of behaviour before making it a df, to save memory space
seg.df.list.fixed.0.4 <- list()
for (i in 1:length(gps.acc.data.list)) {
  data = gps.acc.data.list[[i]]
  print(data$BirdID[1])
  data$obs.id <- paste(data$BirdID, as.numeric(data$date_time), sep = ".")
  seg.df <- create.fixed.segments(segment.length=0.4, data, annotated.data=F, naomit=F)
  seg.df[[1]]$pred.behav <- predict(RF.fixed.0.4, seg.df[[1]])
  seg.df.list.fixed.0.4[[i]] <- merge(unique(seg.df[[2]][,c("BirdID","date_time","altitude","latitude","longitude","speed_2d","segment.id.cut")]), seg.df[[1]])
}

seg.df.list.flex <- list()
for (i in 1:length(gps.acc.data.list)) {
  data = gps.acc.data.list[[i]]
  print(data$BirdID[1])
  data$obs.id <- paste(data$BirdID, as.numeric(data$date_time), sep = ".")
  seg.df <- create.flexible.segments(ARL0=100, data, annotated.data=F, naomit=F)
  seg.df[[1]]$pred.behav <- predict(RF.flexible.100, seg.df[[1]])
  seg.df.list.flex[[i]] <- merge(unique(seg.df[[2]][,c("BirdID","date_time","altitude","latitude","longitude","speed_2d","segment.id.cut")]), seg.df[[1]])
} 

# change from list to dataframe (only works when lists have more than one element):
df.all.fixed.0.4 <- from.list.to.df(seg.df.list.fixed.0.4)
df.all.flex <- from.list.to.df(seg.df.list.flex)

gdata::keep(df.all.fixed.0.4, df.all.flex, RF.fixed.0.4, RF.flexible.100, bird.data, behaviour.colors, behaviours, sure=T)