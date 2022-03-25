# prepare metadata for videos (and rename videos manually)
metadata_videos <- data.frame(video.name=c("584_20130618","763_20130814","763_20130817_1","763_20130817_2","763_20130817_3","760_20140608","763_20140808_1","656_20140808","763_20140808_2","1608_20150624"), BirdID=c(584,763,763,763,763,760,763,656,763,1608), Date=c("2013-06-18","2013-08-14","2013-08-17","2013-08-17","2013-08-17","2014-06-08","2014-08-08","2014-08-08","2014-08-08","2015-06-24"), Start.time=c("08:46:20.00","17:09:40.20","16:53:59.00","17:13:40.72","17:35:01.00","11:59:48.00","09:38:07.00","12:40:19.00","13:38:56.00","04:55:21.32"), End.time=c("09:01:07.00","17:18:27.00","17:01:12.00","17:22:11.42","17:56:21.00","13:24:47.00","10:02:08.00","13:26:21.00","14:31:21.00","05:44:40.02"))

# Import annotated acceleration data (through filming in the wild)
load("data/raw/video_annotated_data.RData")

# prepare raw data in csv files:
# some data from 760 got erroneously imported twice, therefore perform the unique function to keep only unique data:
AnAccDataAll <- unique(AnAccDataAll)
AnAccDataAll <- subset(AnAccDataAll, select==1)[,1:9] # 114769 obs
# link with metadata_videos to add the video name and time.on.video
AnAccDataAll$date.time <- mdy_hms(paste(AnAccDataAll$Date, AnAccDataAll$Time))
AnAccDataAll$date.time.acc <- AnAccDataAll$date.time + seconds(AnAccDataAll$Index/20) # add decimal seconds
AnAccDataAll$date <- mdy(AnAccDataAll$Date)
metadata_videos$date <- ymd(metadata_videos$Date)
metadata_videos$date.time.start <- ymd_hms(paste(metadata_videos$Date, metadata_videos$Start.time))
metadata_videos$date.time.end <- ymd_hms(paste(metadata_videos$Date, metadata_videos$End.time))

acc.with.video.info.all <- merge(AnAccDataAll, metadata_videos[,c("BirdID","date","video.name","date.time.start","date.time.end")], by=c("BirdID","date"), all.x=T)
acc.with.video.info.all$include <- ifelse(acc.with.video.info.all$date.time.acc>=acc.with.video.info.all$date.time.start & acc.with.video.info.all$date.time.acc<=acc.with.video.info.all$date.time.end,1,0)
acc.with.video.info <- acc.with.video.info.all[acc.with.video.info.all$include==1,] # 200 samples smaller than AnAccDataAll
# which data are kicked out? 
datacheck <- merge(AnAccDataAll, acc.with.video.info[,c("BirdID","date.time.acc","Index","include")], by=c("BirdID","date.time.acc","Index"), all.x=T)
excluded.data <- datacheck[is.na(datacheck$include),] # these are data from 763, 2014-08-08 09:36:52 - 2014-08-08 09:37:01. There is no video available during this time frame (the video starts at 09:38:07)! 

# I will remove these data as it is unclear how these were annotated. 
# calculate the time on video (relative to the start of the video): 
acc.with.video.info <- acc.with.video.info[,c(names(AnAccDataAll),"video.name","date.time.start")]
acc.with.video.info$time.on.video <- mdy_hms(paste(acc.with.video.info$Date, "00:00:00"))+difftime(acc.with.video.info$date.time.acc, acc.with.video.info$date.time.start)

# check calculated duration of videos with real duration
metadata_videos$duration <- round(difftime(metadata_videos$date.time.end, metadata_videos$date.time.start),2)
metadata_videos$from_decimal_mins_to_secs <- round(as.numeric(metadata_videos$duration-floor(metadata_videos$duration))*60,0) # check duration with video files
# video 763_20140808_1 is based on given start and end time 1 second shorter than it is in reality. I checked the video, and the start time seems correct. Finetuning of synchronization of video and accelerometer data is done in Matlab. 

# save processed video-annotated acc data
write.csv(acc.with.video.info[,c(names(AnAccDataAll)[1:10],"video.name","time.on.video")], "data/processed/video_annotated_data.csv", row.names=F)

# save behaviour definitions into csv file
behaviours$description <- as.character(behaviours$description)
behaviours$description[behaviours$description=="other"]<-"stand-other"
behaviours$description[behaviours$description=="behav-15"]<-"stand-shake-feathers" # while standing
behaviours$description[behaviours$description=="behav-16"]<-"drink"
names(behaviours)[2] <- "behaviour"
behaviours <- behaviours[1:16,]
write.csv(behaviours[,1:2], "data/raw/behaviours.csv", row.names=F)

# Import migration data of 6283, 6287 and 6291 and save as csv files
load("data/raw/migration_data_for_visual_annotation_soaring_flight.RData") 
write.csv(acc.speed.6283[,1:8], "data/raw/acc_data_migration_6283.csv", row.names=F)
write.csv(acc.speed.6287[,1:8], "data/raw/acc_data_migration_6287.csv", row.names=F)
write.csv(acc.speed.6291[,1:8], "data/raw/acc_data_migration_6291.csv", row.names=F)