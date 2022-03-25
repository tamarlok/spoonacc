# check performance for the flexible segmentation method with different sampling durations 
### Run the model with flexible segmentation, including GPS speed, but first splitting the 10 sec segments into smaller segments (to reflect the length of the segments at which the data is collected on the majority of transmitters, which for spoonbills is 0.8 or 1.6 sec)
ARL.values <- c(500, 5000) # can be extended to compare for multiple ARL values
max.seg.lengths <- c(0.4,0.8,1.6,2,5,10)
# arrays to save results using old segmentation script
sensitivity.flexible.ARL.maxlength.old <- array(NA, c(length(ARL.values),length(behaviour.labels),length(max.seg.lengths)), dimnames = list(ARL.values, behaviour.labels, max.seg.lengths))
precision.flexible.ARL.maxlength.old <-array(NA, c(length(ARL.values),length(behaviour.labels),length(max.seg.lengths)), dimnames = list(ARL.values, behaviour.labels, max.seg.lengths))
# arrays to save results using new segmentation script
sensitivity.flexible.ARL.maxlength <- array(NA, c(length(ARL.values),length(behaviour.labels),length(max.seg.lengths)), dimnames = list(ARL.values, behaviour.labels, max.seg.lengths))
precision.flexible.ARL.maxlength <-array(NA, c(length(ARL.values),length(behaviour.labels),length(max.seg.lengths)), dimnames = list(ARL.values, behaviour.labels, max.seg.lengths))

# make a loop to run for the different max.seg.lengths and ARL-values #
for (i in 1:length(max.seg.lengths)) {
  for (j in 1:length(ARL.values)) {
    # old script
    dfs <- create.flexible.segments(ARL.values[j], max.segment.length=max.seg.lengths[i], startup=20, segmentation.script="old")
    RF.model.output <- RF.model(dfs[[1]], dfs[[2]])
    performance.stats <- calculate.performance(RF.model.output[[2]])
    sensitivity.flexible.ARL.maxlength.old[j,,i] <- performance.stats[[1]]
    precision.flexible.ARL.maxlength.old[j,,i] <- performance.stats[[3]]
    # new script
    dfs <- create.flexible.segments(ARL.values[j], max.segment.length=max.seg.lengths[i], startup=20, segmentation.script="new")
    RF.model.output <- RF.model(dfs[[1]], dfs[[2]])
    performance.stats <- calculate.performance(RF.model.output[[2]])
    sensitivity.flexible.ARL.maxlength[j,,i] <- performance.stats[[1]]
    precision.flexible.ARL.maxlength[j,,i] <- performance.stats[[3]]
    print(round(rbind(sensitivity.flexible.ARL.maxlength.old[j,"forage-intake",], sensitivity.flexible.ARL.maxlength[j,"forage-intake",]),2))
  }
}

# Exploring effect of startup values
# Investigating effect of start up value using max segment length of 1.6
startup.values <- c(1, 10, 20, 30, 40) # can be extended to compare for multiple ARL values
# matrices to save results 
sensitivity.effect.startup.value <- matrix(nrow=length(startup.values), ncol=length(behaviour.labels), dimnames = list(startup.values, behaviour.labels))
precision.effect.startup.value <- matrix(nrow=length(startup.values), ncol=length(behaviour.labels), dimnames = list(startup.values, behaviour.labels))
for (i in 1:length(startup.values)) {
  dfs <- create.flexible.segments(startup=startup.values[i], segmentation.script="new", max.segment.length=1.6)
  RF.model.output <- RF.model(dfs[[1]], dfs[[2]])
  performance.stats <- calculate.performance(RF.model.output[[2]])
  sensitivity.effect.startup.value[i,] <- performance.stats[[1]]
  precision.effect.startup.value[i,] <- performance.stats[[3]]
  print(round(sensitivity.effect.startup.value,2))
}

# explore how the flexible segmentation works for different startup values
# from what I read in the manual (Ross 2015, J Stat Software), the startup value determines the number of observations at the start of the sequence during which no break point is estimated. As I understand it, this means that the first part of the sequence should consist of at least "the value of startup" observations.  

acc.test <- acc.annotated
max.segment.length <- 10

index.min.obs.id <- aggregate(Index~obs.id, acc.test, min)
names(index.min.obs.id)[2]<-"Index.start"
acc.test <- merge(acc.test, index.min.obs.id)
acc.test$Index <- acc.test$Index-acc.test$Index.start

### cut the segments to the length set by max.segment.length to allow the analysed data to better reflect how the data is collected in the long term on the majority of transmitters (in case of the spoonbills, mostly during bouts of 1.6 sec); by default it is set to 10 sec, which is the longest sampling duration in the data (10 sec), used for birds that were video-recorded: 
acc.test$obs.id.cut <- paste(acc.test$obs.id, formatC(format="d", ceiling((acc.test$Index+1)/(max.segment.length*20)),flag="0",width=ceiling(log10(max(ceiling((acc.test$Index+1)/(max.segment.length*20)))))), sep = ".") 

acc.test$segment.id.cut <- acc.test$obs.id.cut
acc.test$duration <- 1/20

un.obs.cut <- aggregate(duration~obs.id.cut, acc.test, sum)
un.obs.cut <- un.obs.cut[round(un.obs.cut$duration,1)==max.segment.length,] # only select segments equal to the max segment length (as this will be the segment length at which the data is collected)
# it could also be set to include smaller segment lengths though... to not loose any data.
un.obs.cut <- un.obs.cut$obs.id.cut
acc.test <- acc.test[acc.test$obs.id.cut %in% un.obs.cut, ]

obs.id.cut.with.handling <- unique(acc.test$obs.id.cut[acc.test$behaviour.index==12])
temp.acc <- acc.test[acc.test$obs.id.cut==obs.id.cut.with.handling[1],]

processStream(temp.acc$x, "GLR", ARL0=500, startup=20) # first changepoint at 32 (32  67  85 117 129 145 166 192)
processStream(temp.acc$x, "GLR", ARL0=500, startup=1) # first changepoint at 32; exactly the same
processStream(temp.acc$x[20:length(temp.acc$x)], "GLR", ARL0=500, startup=1) # first changepoint at 14 (14  48  66  98 110 126 147 173)
c(14,48,66,98,110,126,147,173) + 19 # first breakpoint at 33 instead of 32. 
processStream(temp.acc$x[20:length(temp.acc$x)], "GLR", ARL0=500, startup=20) # exactly the same as with startup = 1; I had expected that a changepoint at 14 would not be estimated with startup set at 20. 
# from this, I conclude that the startup value is not used in the function at all.

rm(acc.test, dfs, RF.model.output)