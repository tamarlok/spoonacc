# load the acceleration data patterns during migration of birds 6283, 6287 and 6291, available at https://doi.org/10.25850/nioz/7b.b.yd
acc.6283 <- read.csv("data/raw/acc_data_migration_6283.csv")
acc.6287 <- read.csv("data/raw/acc_data_migration_6287.csv")
acc.6291 <- read.csv("data/raw/acc_data_migration_6291.csv")
acc.6283$date.time <- ymd_hms(paste(acc.6283$Date, acc.6283$Time))
acc.6287$date.time <- ymd_hms(paste(acc.6287$Date, acc.6287$Time))
acc.6291$date.time <- ymd_hms(paste(acc.6291$Date, acc.6291$Time))

# visual check of 6283 migration data
plot.acc.data.migration.bouts(acc.6283, select.on.speed=T) # select on speed > 2 m/s to select the bouts where the bird was most likely flying. 
# select the bouts that look like passive flight based on the plotted acceleration patterns:
acc.passive.6283 <- acc.6283[acc.6283$date.time=="2016-10-05 10:00:23"|acc.6283$date.time=="2016-10-05 10:39:52"|acc.6283$date.time=="2016-10-05 11:09:28"|acc.6283$date.time=="2016-10-06 10:43:09"|acc.6283$date.time=="2016-10-06 12:52:55"|acc.6283$date.time=="2016-10-06 14:32:15"|acc.6283$date.time=="2016-10-06 14:42:06"|acc.6283$date.time=="2016-10-06 15:02:07"|acc.6283$date.time=="2016-10-06 16:01:32"|acc.6283$date.time=="2016-10-06 16:31:08"|acc.6283$date.time=="2016-10-06 17:31:08"|acc.6283$date.time=="2016-10-09 06:02:07"|acc.6283$date.time=="2016-10-10 12:20:22"|acc.6283$date.time=="2016-10-10 15:48:54"|acc.6283$date.time=="2016-10-11 17:29:57"|acc.6283$date.time=="2016-10-12 02:36:23"|acc.6283$date.time=="2016-10-12 12:41:23"|acc.6283$date.time=="2016-10-12 14:10:39"|acc.6283$date.time=="2016-10-13 21:00:07"|acc.6283$date.time=="2016-10-13 21:09:59"|acc.6283$date.time=="2016-10-13 22:09:10"|acc.6283$date.time=="2016-10-13 22:38:46",]
plot.acc.data.migration.bouts(acc.passive.6283) # plot selected passive flight bouts of 6283
graphics.off() # close all graphical windows

# visual check of 6287 migration data
plot.acc.data.migration.bouts(acc.6287, select.on.speed=T)
acc.passive.6287 <- acc.6287[acc.6287$date.time=="2016-09-11 10:09:31"|acc.6287$date.time=="2016-09-11 10:19:21"|acc.6287$date.time=="2016-09-11 10:29:15"|acc.6287$date.time=="2016-09-11 12:40:41"|acc.6287$date.time=="2016-09-12 16:23:05"|acc.6287$date.time=="2016-09-12 16:33:13"|acc.6287$date.time=="2016-09-17 08:03:16"|acc.6287$date.time=="2016-09-17 09:13:07"|acc.6287$date.time=="2016-09-17 15:10:40"|acc.6287$date.time=="2016-09-17 17:09:52"|acc.6287$date.time=="2016-09-18 13:03:50"|acc.6287$date.time=="2016-09-18 13:14:17"|acc.6287$date.time=="2016-09-18 21:52:21"|acc.6287$date.time=="2016-09-19 01:40:52"|acc.6287$date.time=="2016-09-19 03:10:58",]
plot.acc.data.migration.bouts(acc.passive.6287) # plot selected passive flight bouts of 6287
graphics.off() # close all graphical windows

# visual check of 6291 migration data
plot.acc.data.migration.bouts(acc.6291, select.on.speed=T) 
acc.passive.6291 <- acc.6291[acc.6291$date.time=="2016-09-21 17:39:52"|acc.6291$date.time=="2016-09-24 12:59:26"|acc.6291$date.time=="2016-09-24 13:09:18"|acc.6291$date.time=="2016-09-24 13:38:54"|acc.6291$date.time=="2016-09-25 09:42:46"|acc.6291$date.time=="2016-09-25 10:42:13"|acc.6291$date.time=="2016-09-25 11:01:57"|acc.6291$date.time=="2016-09-25 11:51:34"|acc.6291$date.time=="2016-09-25 12:12:04"|acc.6291$date.time=="2016-09-25 13:12:05"|acc.6291$date.time=="2016-09-25 13:31:50"|acc.6291$date.time=="2016-09-25 14:32:00"|acc.6291$date.time=="2016-09-26 10:38:20"|acc.6291$date.time=="2016-09-26 23:44:55",]
plot.acc.data.migration.bouts(acc.passive.6291) # plot selected passive flight bouts of 6291
graphics.off() # close all graphical windows

acc.passive <- rbind(acc.passive.6283, acc.passive.6287, acc.passive.6291)
acc.passive$behaviour.index <- 9 # behaviour index for passive flight
acc.passive <- acc.passive[,c(1:8,10,9)] # change the order of the columns, for combining with video annotated data

write.csv(acc.passive, "data/processed/visual_annotated_passive_flight_data.csv", row.names=F)
