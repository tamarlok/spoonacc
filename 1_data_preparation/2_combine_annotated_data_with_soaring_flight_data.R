# load graphically-annotated flight data (as produced by preceding script): 
acc.soaring <- read.csv("data/processed/visual_annotated_passive_flight_data.csv") # available at https://doi.org/10.25850/nioz/7b.b.yd
acc.soaring$annotation.method <- "visual"

# load video-annotated data
acc.video <- read.csv("data/raw/video_annotated_data.csv")
acc.video <- acc.video[,1:10] # remove the video information columns, not needed for analysis
# load list of behaviours distinguished during video annotation:
behaviours <- read.csv("data/raw/behaviours.csv") # available at https://doi.org/10.25850/nioz/7b.b.yd
acc.video$annotation.method <- "video"
acc.annotated <- rbind(acc.video, acc.soaring) # combine visual and video annotated data
acc.annotated$date.time <- ymd_hms(acc.annotated$date.time)

# assign id's
acc.annotated$obs.id <- paste(acc.annotated$BirdID, as.numeric(acc.annotated$date.time), sep = ".")
acc.annotated$segment.id <- paste(acc.annotated$obs.id, ".a", sep = "")
acc.annotated$ind.id <- paste(acc.annotated$obs.id, ".", acc.annotated$Index, sep = "")

# add column with behaviour names
acc.annotated$behaviour <- as.character(behaviours$behaviour[match(acc.annotated$behaviour.index, behaviours$behaviour.index)])

# order data acc.annotatedording to BirdID, date.time and Index
acc.annotated <- acc.annotated[order(acc.annotated$BirdID, acc.annotated$date.time, acc.annotated$Index),]

table(acc.annotated$Index) # index is max. 201 (mostly 0-199, i.e. 200 points), implying 10 seconds of 20 Hz acc.annotated data. 

# rename behaviours so they appear in logical order in the graph:
acc.annotated$behaviour1 <- acc.annotated$behaviour
acc.annotated$behaviour1[acc.annotated$behaviour1=="stand-rest"]<-"01.stand-rest"
acc.annotated$behaviour1[acc.annotated$behaviour1=="stand-alert"]<-"02.stand-alert"
acc.annotated$behaviour1[acc.annotated$behaviour1=="stand-preen"]<-"03.stand-preen"
acc.annotated$behaviour1[acc.annotated$behaviour1=="stand-other"]<-"04.stand-other"
acc.annotated$behaviour1[acc.annotated$behaviour1=="stand-shake-feathers"]<-"05.shake-feathers"
acc.annotated$behaviour1[acc.annotated$behaviour1=="sit-alert"]<-"06.sit-alert"
acc.annotated$behaviour1[acc.annotated$behaviour1=="fly-flap"]<-"07.fly-active"
acc.annotated$behaviour1[acc.annotated$behaviour1=="fly-soar"]<-"08.fly-passive"
acc.annotated$behaviour1[acc.annotated$behaviour1=="walk"]<-"09.walk"
acc.annotated$behaviour1[acc.annotated$behaviour1=="for-search"]<-"10.search"
acc.annotated$behaviour1[acc.annotated$behaviour1=="for-handle"]<-"11.handle"
acc.annotated$behaviour1[acc.annotated$behaviour1=="for-intake"]<-"12.ingest"
acc.annotated$behaviour1[acc.annotated$behaviour1=="drink"]<-"13.drink"

# create column with behaviours pooled into 9 classes:
acc.annotated$behaviour2 <- acc.annotated$behaviour1
acc.annotated$behaviour2[acc.annotated$behaviour=="stand-other"|acc.annotated$behaviour=="stand-shake-feathers"] <- "01.stand"
acc.annotated$behaviour2[acc.annotated$behaviour=="stand-alert"|acc.annotated$behaviour=="stand-preen"|acc.annotated$behaviour=="stand-rest"] <- "01.stand"
acc.annotated$behaviour2[acc.annotated$behaviour=="sit-alert"] <- "06.sit"

acc.annotated$behaviour.pooled <- substr(acc.annotated$behaviour2,4,15) # same as behaviour2, but without the numbering

acc.annotated$behaviour1 <- as.factor(acc.annotated$behaviour1)
acc.annotated$behaviour2 <- as.factor(acc.annotated$behaviour2)

## gives NAs in acc.annotatedelerometer measurements the value of the previous measurement
acc.annotated$x[which(is.na(acc.annotated$x))] <- acc.annotated$x[which(is.na(acc.annotated$x))-1]
acc.annotated$y[which(is.na(acc.annotated$y))] <- acc.annotated$y[which(is.na(acc.annotated$y))-1]
acc.annotated$z[which(is.na(acc.annotated$z))] <- acc.annotated$z[which(is.na(acc.annotated$z))-1]

# Create predictor variable names
predictors.all <- c("mean.x", "mean.y", "mean.z", "min.x", "min.y", "min.z", 
                    "max.x", "max.y", "max.z", "trend.x", "trend.y", "trend.z", 
                    "odba.x", "odba.y", "odba.z", "odba", "dps.x", "dps.y", "dps.z", 
                    "fdps.x", "fdps.y", "fdps.z", "kurt.x", "kurt.y", "kurt.z", 
                    "skew.x", "skew.y", "skew.z", "noise.x", "noise.y", "noise.z", 
                    "speed_2d")

# Create behaviour names for pooled behaviours
behaviour.pooled <- c("drink", "fly-active","fly-passive","handle","ingest","search","sit","stand","walk")
