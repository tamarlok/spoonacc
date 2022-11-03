#
load("data/raw/imported.gps.acc.data.2016-2019_20210929.RData") # includes GPS, ACC and device info data imported from UvABiTS for each bird seperately

# Individuals with data from 2016 onward, in the same order as the birds are in the above imported lists.
logger <- NULL
for (i in 1:length(gps.data.list)) logger[i] <- gps.data.list[[i]]$device_info_serial[1]
sex <- c("M","F","F","F","M","M","F","F","F","F","F","M","M","M","M","M","M")
year.start <- c(rep(2016,9),rep(2017,3),2018,rep(2016,4))
year.end <- c(2017, 2020, 2017, 2017, rep(2020, 13))
bird.data <- data.frame(logger,sex,year.start,year.end)
write.csv(bird.data, "data/raw/bird.data.application.csv", row.names=F)

# link gps and acc data, while keeping the list structure in which it was downloaded from the UvA BiTS database. This reduces the required RAM. 
# add column with BirdID to each item in the list, adjust Index to start at 0 and sort on date_time and Index (for the segmentation code to work properly)
gps.acc.data.list <- list()
for (i in 1:length(gps.data.list)) {
  gps.acc.data.list[[i]] <- link.gps.acc.data(gps.data=na.omit(gps.data.list[[i]]), acc.data=na.omit(acc.data.list[[i]]), device.info=device.infos[i,])
  gps.acc.data.list[[i]]$BirdID <- paste(bird.data$logger[i], bird.data$year.start[i],sep="_")
  gps.acc.data.list[[i]]$Index <- gps.acc.data.list[[i]]$Index-1 # for the segmentation code to work properly, Index should start at 0
  gps.acc.data.list[[i]] <- gps.acc.data.list[[i]][order(gps.acc.data.list[[i]]$date_time, gps.acc.data.list[[i]]$Index),]
}

rm(gps.data.list, acc.data.list)
save.image("data/processed/gps.acc.data.list.RData")

# make separate csv files for each bird's gps.acc.data 
for (i in 1:length(gps.acc.data.list)) {
  filename <- paste("data/processed/gps.acc.data.",logger[i],"_",year.start[i],".csv", sep="")
  write.csv(gps.acc.data.list[[i]], filename, row.names=F)
}
