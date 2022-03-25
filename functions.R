# plot all acceleration samples for a selected range of dates during migration for visual annotation of soaring flight
plot.acc.data.migration.bouts <- function(acc, date.start = min(acc$date.time), date.end = max(acc$date.time), select.on.speed = F) {
  acc <- acc[order(acc$date.time, acc$Index),]
  # only plot selected range of dates, i.e. in this case during migration)
  acc.sel <- acc[acc$date.time>=date.start & acc$date.time<=date.end,] 
  # if select.on.speed is TRUE, only show bouts where speed > 2 m/s. 
  if (select.on.speed==T) acc.sel <- acc.sel[acc.sel$speed_2d>2,]  
  count <- 1
  windows(12,7)
  layout(matrix(1:32,ncol=8,byrow=T))
  par(mar=c(1,1,0,0),oma=c(3.5,3.5,1,1))
  for (i in unique(acc.sel$date.time)) {
    plot(x~Index, acc.sel[acc.sel$date.time==i,], col="red", type="l", xlim=c(1,32), ylim=c(-3,3), xaxt='n', yaxt='n')
    lines(y~Index, acc.sel[acc.sel$date.time==i,], col="blue")
    lines(z~Index, acc.sel[acc.sel$date.time==i,], col="green")
    text(1,-2.8,unique(acc.sel$date.time[acc.sel$date.time==i]), adj=0)
    text(1,2.8,paste("speed =", round(unique(na.omit(acc.sel$speed_2d[acc.sel$date.time==i])),1), "m/s", sep=" "), adj=0)
    if(count<25) axis(1, at=c(0,0.5,1,1.5)*20+1, labels=F) else axis(1, at=c(0,0.5,1,1.5)*20+1, labels=c(0,0.5,1,1.5))
    if(count%in%c(1,9,17,25)) axis(2, at=seq(-3,3,1), labels=T) else axis(2, at=seq(-3,3,1), labels=F)
    count <- count + 1
    if (count==32 | count==length(unique(acc.sel$date.time)))   {
      mtext("Time (s)",1,2,outer=T)
      mtext("acceleration (g)",2,2,outer=T)
    }
    if (count>32) {
      count <- 1
      windows(12,7)
      layout(matrix(1:32,ncol=8,byrow=T))
      par(mar=c(1,1,0,0),oma=c(3.5,3.5,1,1))
    }
  }
}

#### Make graph of acceleration signal for one selected acceleration sample
plot.acc.panel <- function(acc.data, plot.x=F, plot.y=F, plot.intake=F, behaviour=NA) {
  if (plot.intake==F) plot(x~Index, acc.data, xlim=c(0,31), ylim=c(-3,3), xaxt="n", yaxt="n", type="n")
  if (plot.intake==T) {
    plot(x~Index, acc.data, xlim=c(0,32), ylim=c(-3,3), xaxt="n", yaxt="n", type="n")
    start.intake <- min(acc.data$Index[acc.data$behaviour2=="12.for-intake"])
    end.intake <- max(acc.data$Index[acc.data$behaviour2=="12.for-intake"])
    polygon(c(start.intake, end.intake, end.intake, start.intake), c(-3.21,-3.21,3.2,3.2), col="grey90", border=F) 
    text(mean(c(start.intake, end.intake)), 2.8,"ingest", cex=1.4)
  }
  if (plot.x==T) axis(1,at=c(0,10,20,30),labels=c(0,0.5,1,1.5))
  if (plot.y==T) axis(2, at=seq(-3,3,1), las=1)
  text(0,-2.8,behaviour, adj=0, cex=1.4)
  points(x~Index, acc.data, col="red", pch=19, cex=0.8)
  lines(x~Index, acc.data, col="red")
  points(y~Index, acc.data, col="blue", pch=19, cex=0.8)
  lines(y~Index, acc.data, col="blue")
  points(z~Index, acc.data, col="green", pch=19, cex=0.8)
  lines(z~Index, acc.data, col="green")
}

# Dominant Power Spectrum
dps <- function(x){
  d.x <- x - mean(x, na.rm = T)   
  return(max(((Mod(fft(d.x))^2)/length(x))[1:(length(x)/2)]))
}

# Frequency at the Dominant Power Spectrum
fdps <- function(x){
  fq <- (1:(length(x)/2)) / (length(x)*0.05)
  d.x <- x - mean(x, na.rm = T)   
  return(fq[which.max(((Mod(fft(d.x))^2)/length(x))[1:(length(x)/2)])])
}

odba <- function(x){
  d.x <- x - mean(x, na.rm = T)   
  return(sum(abs(d.x), na.rm = T)/length(x))
}

trend <- function(x){
  dx <- lm(x ~ c(1:length(x)))
  return(dx$coeff[2])
}

noise <- function(x){
  noise.tmp <- NA
  noise.mean <- NA
  if (length(x)>2) {
    for (i in 2:(length(x)-1)) noise.tmp[i] <- x[i]-(x[i-1]+x[i+1])/2
    noise.mean <- mean(na.omit(noise.tmp))
  }
  return(noise.mean)
}

# to make the occurrence of different behaviours similar, downsample for-search and stand in the data
downsampling.behaviours <- function(seg.df, stand=1, search=1) {
  segments.to.keep <- c(seg.df$segment.id.cut[seg.df$behaviour.pooled!="stand"&seg.df$behaviour.pooled!="for-search"],
                        sample(seg.df$segment.id.cut[seg.df$behaviour.pooled=="stand"],length(seg.df$segment.id.cut[seg.df$behaviour.pooled=="stand"])/stand),
                        sample(seg.df$segment.id.cut[seg.df$behaviour.pooled=="for-search"],length(seg.df$segment.id.cut[seg.df$behaviour.pooled=="for-search"])/search))
  seg.df <- seg.df[seg.df$segment.id.cut %in% segments.to.keep,]
  seg.df
}

# in case we also want to upsample certain behaviours, it is important to only do that in the training dataset. 
upsampling.behaviours <- function(seg.df, drink=1, handle=1, ingest=1, walk=1, soar=1) {
  drink.resampled.rows <- gdata::resample(which(seg.df$behaviour.pooled=="drink"), size=drink*dim(seg.df[seg.df$behaviour.pooled=="drink",])[1], replace=TRUE)
  handle.resampled.rows <- gdata::resample(which(seg.df$behaviour.pooled=="for-handle"), size=handle*dim(seg.df[seg.df$behaviour.pooled=="for-handle",])[1], replace=TRUE)
  ingest.resampled.rows <- gdata::resample(which(seg.df$behaviour.pooled=="for-intake"), size=ingest*dim(seg.df[seg.df$behaviour.pooled=="for-intake",])[1], replace=TRUE)
  walk.resampled.rows <- gdata::resample(which(seg.df$behaviour.pooled=="walk"), size=walk*dim(seg.df[seg.df$behaviour.pooled=="walk",])[1], replace=TRUE)
  soar.resampled.rows <- gdata::resample(which(seg.df$behaviour.pooled=="fly-soar"), size=soar*dim(seg.df[seg.df$behaviour.pooled=="fly-soar",])[1], replace=TRUE)
  seg.df.upsampled <- seg.df[c(1:dim(seg.df)[1],drink.resampled.rows,handle.resampled.rows,ingest.resampled.rows,walk.resampled.rows,soar.resampled.rows),]
  seg.df.upsampled
}

################################################################
## Function to do segmentation for a specified segment length ##
################################################################
create.fixed.segments <- function(segment.length, acc = acc.annotated, remove.shorter.segments = TRUE, sampling.freq=20, annotated.data=T, naomit=T) { # segment length is expressed in seconds
  
  samples.per.segment <- ceiling(segment.length * sampling.freq) # to make it an integer (and select only segments with highest possible number of samples, given the sampling frequency; e.g. for a segment of 0.8 seconds at 2 Hz, this is 2 samples)
  
  # 20220117: check whether indices start at 0 or 1 
  #min.index.per.sample <- aggregate(Index~Date+Time+BirdID, acc, min) # the column Time does not exist in the non-annotated data, so gives an error when applied to these data.
  #table(min.index.per.sample$Index) # we could start these samples all at 0 by substracting the min index, but this is not yet implemented.
  
  # 20210617: these two lines are added to include the adjustment of the original sampling frequency (which was 20 Hz) within the creation of fixed segments code:
  indices.to.use <- seq(min(acc$Index),max(acc$Index),by=20/sampling.freq)
  acc.sel = acc[acc$Index %in% indices.to.use,] 

  acc.sel$segment.id.cut <- paste(acc.sel$obs.id, formatC(format="d", ceiling((acc.sel$Index+1)/(segment.length*20)),flag="0",width=ceiling(log10(max(ceiling((acc.sel$Index+1)/(segment.length*20)))))), sep = ".") # 20 is the sampling frequency at which the data was originally collected
  
  ## calculate summary statistics for each segment: 
  seg.df <- ddply(acc.sel, .(segment.id.cut, BirdID), summarize, 
                  nobs.segments  = length (x), speed_2d = mean(speed_2d),
                  mean.x = mean(x), mean.z = mean(z), mean.y = mean(y), 
                  min.x = min (x), min.y = min (y), min.z = min (z),
                  max.x = max (x), max.y = max (y), max.z = max (z), 
                  trend.x = trend (x), trend.y = trend (y), trend.z = trend (z),
                  odba.x = odba(x), odba.y = odba(y), odba.z = odba(z), 
                  dps.x = dps(x), dps.y = dps(y), dps.z = dps(z),
                  fdps.x = fdps(x),  fdps.y = fdps(y), fdps.z = fdps(z), 
                  kurt.x = kurtosis(x), kurt.y = kurtosis(y), kurt.z = kurtosis(z), 
                  skew.x = skewness(x), skew.y = skewness(y), skew.z = skewness(z),
                  noise.x = noise(x), noise.y = noise(y), noise.z = noise(z)
  ) 
  
  seg.df$odba <- seg.df$odba.x + seg.df$odba.y + seg.df$odba.z 

  # If remove.shorter.segments is set at TRUE, then only use the segments of specified segment length for the machine learning and testing: 
  if (remove.shorter.segments == TRUE) seg.df.sel <- seg.df[seg.df$nobs.segments==samples.per.segment,] 
  set.seed(3) # to reproduce the same results from the random choice of equally occurring behaviours below (not sure if this is passed onto the global environment; this should not happen!)
  if (annotated.data==T) seg.df.sel <- assign.behaviour.to.segments(seg.df.sel, acc.sel) # here is some randomness included, when there are two behaviours expressed an equal amount of time within a single segment. One of these behaviours is then randomly chosen.
  table(seg.df.sel$behaviour.pooled)
  if (naomit==T) list(na.omit(seg.df.sel), acc.sel) else list(seg.df.sel, acc.sel) # remove cases where certain predictor variables (e.g., kurtosis or skewness) could not be calculated, if naomit=T
}

#########################################################
## Function to create dataframe with flexible segments ##
#########################################################

## check whether this works with downsampling the sampling frequency (see above for fixed segment lengths)
create.flexible.segments <- function(ARL0=5000, acc = acc.annotated, sampling.freq=20, max.segment.length=1.6, segmentation.script = "new", startup=1, annotated.data=T, naomit=T) {
  
  ### START creation flexible segments ###
  
  ## Renumber column Index so that each obs.id starts at 0
  index.min.obs.id <- aggregate(Index~obs.id, acc, min)
  names(index.min.obs.id)[2]<-"Index.start"
  acc <- merge(acc, index.min.obs.id)
  acc$Index <- acc$Index-acc$Index.start

  ### cut the segments to the length set by max.segment.length to allow the analysed data to better reflect how the data is collected in the long term on the majority of transmitters (in case of the spoonbills, mostly during bouts of 1.6 sec); by default it is set to 10 sec, which is the longest sampling duration in the data (10 sec), used for birds that were video-recorded: 
  acc$obs.id.cut <- paste(acc$obs.id, formatC(format="d", ceiling((acc$Index+1)/(max.segment.length*20)),flag="0",width=ceiling(log10(max(ceiling((acc$Index+1)/(max.segment.length*20)))))), sep = ".") 
  
  acc$segment.id.cut <- acc$obs.id.cut
  acc$duration <- 1/20
  
  un.obs.cut <- aggregate(duration~obs.id.cut, acc, sum)
  
  un.obs.cut <- un.obs.cut[round(un.obs.cut$duration,1)==max.segment.length,] # only select segments equal to the max segment length (as this will be the segment length at which the data is collected)
  un.obs.cut <- un.obs.cut$obs.id.cut
  acc <- acc[acc$obs.id.cut %in% un.obs.cut, ]
  
  # new script:
  if (segmentation.script == "new") { 
    for(k in 1 : length(un.obs.cut)) {
    temp.acc <- acc[which(acc$obs.id.cut == un.obs.cut[k]),]
    
    dcpb <- processStream(temp.acc$x, "GLR", ARL0=ARL0, startup=startup)   
    
    incl <- dcpb$changePoints[c(1,which(diff(dcpb$changePoints)>2)+1)] # after selecting the first changepoint, only select subsequent changepoints that are more than 2 acc-samples further from the previous changepoint (i.e. causing the minimum segment length to become 3 samples); Bom et al. 2014 used a min sample size of 4 here. If breakpoints are estimated at position 3, 5 and 7 for example, only 3 is used.  
    
    if (is.na(max(incl))==F) # only run the below line when there is at least one change point, otherwise use the original (and entire) segment.id.cut. 
        acc$segment.id.cut[which(acc$obs.id.cut == un.obs.cut[k])] <- paste(temp.acc$obs.id.cut, letters[rep(c(1:(length(incl)+1)), c(diff(c(0, incl, nrow(temp.acc)))))], sep = ".")
    }
  }
    
   # old script used in Bom et al. 2014 (performs particularly worse with segment lengths of 1-2 s; the new script is insensitive to max seg length)
   if (segmentation.script == "old") {
     for(k in 1 : length(un.obs.cut)) {
      temp.acc <- acc[which(acc$obs.id.cut == un.obs.cut[k]),]
      
      dcpb <- processStream(temp.acc$x,"GLR",ARL0=ARL0, startup=startup)
      
      incl <- dcpb$changePoints[c(1,which(diff(dcpb$changePoint)>3)+1)]
      if(length(incl) > 1) # this code causes the segments to be cut into smaller segments only when there is more than one breakpoint, whereas it should also be cut when there is only one breakpoint. This causes the dip in performance at max segment lengths of 1-2 s.
        acc$segment.id.cut[which(acc$obs.id.cut == un.obs.cut[k])] <- paste(temp.acc$obs.id.cut, letters[rep(c(1:(length(incl)+1)), c(diff(c(0, incl, nrow(temp.acc)))))], sep = ".")
    }
  }

  # segments can be shorter but never larger than the sampling duration * sampling frequency:
  max.segment.length * sampling.freq
  min(table(acc$segment.id.cut))
  max(table(acc$segment.id.cut))
  
  ### END creation flexible segments ###
  
  # remove NA's from acc:
  if (annotated.data==T) acc <- na.omit(acc[,c("BirdID","segment.id.cut","speed_2d","x","y","z","behaviour.pooled")]) else acc <- na.omit(acc[,c("BirdID","segment.id.cut","device_info_serial","date_time","latitude","longitude","altitude","speed_2d","x","y","z")])
  
  ## calculate summary statistics per segment
  seg.df <- ddply(acc, .(segment.id.cut, BirdID), summarize, 
                  nobs.segments  = length (x), speed_2d = mean(speed_2d),
                  mean.x = mean(x), mean.z = mean(z), mean.y = mean(y), 
                  min.x = min (x), min.y = min (y), min.z = min (z),
                  max.x = max (x), max.y = max (y), max.z = max (z), 
                  trend.x = trend (x), trend.y = trend (y), trend.z = trend (z),
                  odba.x = odba(x), odba.y = odba(y), odba.z = odba(z), 
                  dps.x = dps(x), dps.y = dps(y), dps.z = dps(z),
                  fdps.x = fdps(x),  fdps.y = fdps(y), fdps.z = fdps(z), 
                  kurt.x = kurtosis(x), kurt.y = kurtosis(y), kurt.z = kurtosis(z), 
                  skew.x = skewness(x), skew.y = skewness(y), skew.z = skewness(z),
                  noise.x = noise(x), noise.y = noise(y), noise.z = noise(z)
  ) 
  
  seg.df$odba <- seg.df$odba.x + seg.df$odba.y + seg.df$odba.z 
  if (annotated.data==T) seg.df <- assign.behaviour.to.segments(seg.df, acc)
  if (naomit==T) list(na.omit(seg.df), acc) # remove cases where certain predictor variables (e.g., kurtosis or skewness) could not be calculated
  else list(seg.df, acc)
}  

assign.behaviour.to.segments <- function(seg.df, acc) {
    # match most occurring behaviour during a segment with seg.df data frame (this is different from Roeland's code)
    acc$freq <- 1
    segment.id.cut.behaviours <- aggregate(freq~segment.id.cut+behaviour.pooled, acc, sum)
    segment.id.cut.nobsmax <- aggregate(freq~segment.id.cut, segment.id.cut.behaviours, max) # the number of points that the longest expressed behaviour is expressed
    segment.id.cut.behavdom <- merge(segment.id.cut.behaviours, segment.id.cut.nobsmax, by=c('segment.id.cut','freq')) # behaviour that is expressed the longest
    # randomly select one of the equally often expressed behaviours:
    segment.id.cut.behavdom$rnd <- runif(dim(segment.id.cut.behavdom)[1])
    segment.id.cut.rndmax <- aggregate(rnd~segment.id.cut, segment.id.cut.behavdom, max)
    segment.id.cut.behavsel <- merge(segment.id.cut.behavdom, segment.id.cut.rndmax, by=c("segment.id.cut","rnd"))
    
    # determine whether a segment consists of a single behaviour ("clean" segments)
    num.obs <- cast(melt(acc), segment.id.cut ~ behaviour.pooled, length, subset = variable == 'BirdID')
    nobs.segment <- aggregate(freq~segment.id.cut, acc, sum)
    names(nobs.segment)[2]<-"nobs"
    segment.id.cut.behavsel <- merge(segment.id.cut.behavsel, nobs.segment, all.x=T)
    segment.id.cut.behavsel$single.behaviour <- 0
    segment.id.cut.behavsel$single.behaviour[segment.id.cut.behavsel$freq==segment.id.cut.behavsel$nobs] <- 1
    
    seg.df$behaviour.pooled <- segment.id.cut.behavsel$behaviour.pooled[match(seg.df$segment.id.cut,  segment.id.cut.behavsel$segment.id.cut)] 
    seg.df$single.behaviour <- segment.id.cut.behavsel$single.behaviour[match(seg.df$segment.id.cut,  segment.id.cut.behavsel$segment.id.cut)] 
    seg.df <- seg.df[is.na(seg.df$behaviour.pooled)==F,] # remove cases where behaviour was not classified
    seg.df
  }
     
######################################
## Function for Random Forest model ##
######################################

RF.model <- function(seg.df, acc, selected.variables = predictors.all, clean.segments.train = FALSE, clean.segments.test = FALSE, stand=1, search=1, drink=1, handle=1, ingest=1, walk=1, soar=1) {
  # in the segmentation function, there is an option to remove all rows which contain at least 1 predictor with NA. If this has NOT been done, we here remove the predictors that contain all NA's (because they can't be calculated over 1 or 2 points), and then remove the remaining rows that still have some NA's for other predictors. 
  seg.df <- seg.df[,apply(!is.na(seg.df), 2, any)]
  seg.df <- na.omit(seg.df)
    
  ind <- sample(1:2, nrow(seg.df), replace = TRUE, prob=c(0.7, 0.3)) # divide data into 70% training (ind=1) and 30% testing (ind=2) data 
  data.train <- seg.df[ind == 1,] # the training dataset
  data.test <- seg.df[ind ==2,] # the testing dataset
  
  # perform up- and downsampling on the train dataset only, to be able to properly interpret the effects on sensitivity and precision for the test dataset
  data.train <- downsampling.behaviours(data.train, stand=stand, search=search)
  data.train <- upsampling.behaviours(data.train, drink=drink, handle=handle, ingest=ingest, walk=walk, soar=soar)
  data.train$behaviour.pooled <-  factor(data.train$behaviour.pooled) 
  if (clean.segments.train == T) data.train <- data.train[data.train$single.behaviour==1,]
  if (clean.segments.test == T) data.test <- data.test[data.test$single.behaviour==1,]
  
  # remove the predictor variables from selected.variables that contained all NA in seg.df (e.g. noise when there are only 2 samples per segment, at 2 Hz)
  selected.variables <- selected.variables[selected.variables%in%names(seg.df)]
  data.train <- data.train[,c("behaviour.pooled", selected.variables)]
  
  # fit the model on the train dataset
  fit.RF <- randomForest(behaviour.pooled ~ ., data = data.train, importance=T)
  behav.pred <- predict(fit.RF, data.test) # do the prediction on a random selection of the dataset (the testing/validation dataset)
  df.pred <- cbind(data.test, behav.pred)
  acc.pred  <- merge(acc, df.pred[,c("segment.id.cut","behav.pred")])
  mytable <- table(predicted = acc.pred$behav.pred, observed = acc.pred$behaviour.pooled)
  mytable <- mytable[, match(rownames(mytable), colnames(mytable))]
  list(fit.RF, mytable, df.pred)
}

RF.model.start <- function(seg.df, stand=1, search=1, drink=1, handle=1, ingest=1, walk=1, soar=1) {
  seg.df <- downsampling.behaviours(seg.df, stand=stand, search=search)
  seg.df <- upsampling.behaviours(seg.df, drink=drink, handle=handle, ingest=ingest, walk=walk, soar=soar)
  seg.df$behaviour.pooled <-  factor(seg.df$behaviour.pooled)
  # fit the model on the train dataset
  fit.RF <- randomForest(behaviour.pooled ~ mean.x + mean.y + mean.z + 
                           min.x + min.y + min.z + max.x + max.y + max.z + 
                           trend.x + trend.y + trend.z + odba.x + odba.y + odba.z + odba + 
                           dps.x + dps.y + dps.z + fdps.x + fdps.y + fdps.z + 
                           kurt.x + kurt.y + kurt.z + skew.x + skew.y + skew.z +
                           noise.x + noise.y + noise.z + 
                           speed_2d, 
                         data = seg.df, importance=T)
  fit.RF
}

## Calculate measures of classification performance for behaviour.pooled classes 
calculate.performance <- function(RF.model.results) {
  RF.model.results <- RF.model.results[,is.na(colnames(RF.model.results))==F]
  if (max(colnames(RF.model.results)=="for-intake")==0) RF.model.results <- cbind(RF.model.results,"for-intake"=0)
  if (max(colnames(RF.model.results)=="for-handle")==0) RF.model.results <- cbind(RF.model.results,"for-handle"=0)
  if (max(colnames(RF.model.results)=="fly-soar")==0) RF.model.results <- cbind(RF.model.results,"fly-soar"=0)
  if (max(colnames(RF.model.results)=="drink")==0) RF.model.results <- cbind(RF.model.results,"drink"=0)
  if (max(colnames(RF.model.results)=="stand")==0) RF.model.results <- cbind(RF.model.results,"stand"=0)
  if (max(rownames(RF.model.results)=="for-intake")==0) RF.model.results <- rbind(RF.model.results,"for-intake"=0)
  if (max(rownames(RF.model.results)=="for-handle")==0) RF.model.results <- rbind(RF.model.results,"for-handle"=0)
  if (max(rownames(RF.model.results)=="fly-soar")==0) RF.model.results <- rbind(RF.model.results,"fly-soar"=0)
  if (max(rownames(RF.model.results)=="drink")==0) RF.model.results <- rbind(RF.model.results,"drink"=0)
  if (max(rownames(RF.model.results)=="stand")==0) RF.model.results <- rbind(RF.model.results,"stand"=0)
  
  # order by column and rowname
  RF.model.results <- RF.model.results[order(rownames(RF.model.results)),order(colnames(RF.model.results))]
  
  cM <- confusionMatrix(as.table(RF.model.results))
  names(cM)
  sensitivity <- cM$byClass[,"Sensitivity"] # 
  specificity <- cM$byClass[,"Specificity"] # or recall
  precision <- cM$byClass[,"Pos Pred Value"] # or precision
  accuracy.overall <- sum(diag(RF.model.results))/sum(RF.model.results) # this is the "global" accuracy = (TP+TN)/(TP+FP+FN+TN)
  list(sensitivity, specificity, precision, accuracy.overall)
}
### End of classification performance calculation

calculate.performance.pooled.behaviours <- function(RF.model.results) {
  RF.model.results <- RF.model.results[,is.na(colnames(RF.model.results))==F]
  if (max(colnames(RF.model.results)=="for-intake")==0) RF.model.results <- cbind(RF.model.results,"for-intake"=0)
  if (max(colnames(RF.model.results)=="for-handle")==0) RF.model.results <- cbind(RF.model.results,"for-handle"=0)
  if (max(colnames(RF.model.results)=="fly-soar")==0) RF.model.results <- cbind(RF.model.results,"fly-soar"=0)
  if (max(colnames(RF.model.results)=="drink")==0) RF.model.results <- cbind(RF.model.results,"drink"=0)
  if (max(colnames(RF.model.results)=="stand")==0) RF.model.results <- cbind(RF.model.results,"stand"=0)
  if (max(rownames(RF.model.results)=="for-intake")==0) RF.model.results <- rbind(RF.model.results,"for-intake"=0)
  if (max(rownames(RF.model.results)=="for-handle")==0) RF.model.results <- rbind(RF.model.results,"for-handle"=0)
  if (max(rownames(RF.model.results)=="fly-soar")==0) RF.model.results <- rbind(RF.model.results,"fly-soar"=0)
  if (max(rownames(RF.model.results)=="drink")==0) RF.model.results <- rbind(RF.model.results,"drink"=0)
  if (max(rownames(RF.model.results)=="stand")==0) RF.model.results <- rbind(RF.model.results,"stand"=0)
  
  # order by column and rowname
  RF.model.results <- RF.model.results[order(rownames(RF.model.results)),order(colnames(RF.model.results))]
  # merge active on the ground behaviours: 
  RF.model.results <- cbind(RF.model.results, active.ground=rowSums(RF.model.results[,c("for-handle","for-intake","for-search","drink","walk")]))
  RF.model.results <- rbind(RF.model.results, active.ground=colSums(RF.model.results[c("for-handle","for-intake","for-search","drink","walk"),]))
  # merge resting
  RF.model.results <- cbind(RF.model.results, rest=rowSums(RF.model.results[,c("stand","sit")]))
  RF.model.results <- rbind(RF.model.results, rest=colSums(RF.model.results[c("stand","sit"),]))
  # only keep merged behaviours, and fly-flap and fly-soar:
  RF.model.results <- RF.model.results[c("fly-flap","fly-soar","active.ground","rest"),c("fly-flap","fly-soar","active.ground","rest")]
  
  cM <- confusionMatrix(as.table(RF.model.results))
  names(cM)
  sensitivity <- cM$byClass[,"Sensitivity"] # 
  specificity <- cM$byClass[,"Specificity"] # or recall
  precision <- cM$byClass[,"Pos Pred Value"] # or precision
  accuracy.overall <- sum(diag(RF.model.results))/sum(RF.model.results) # this is the "global" accuracy = (TP+TN)/(TP+FP+FN+TN)
  list(sensitivity, specificity, precision, accuracy.overall)
}


### Calculate how much estimated intake rate deviates from observed intake rate, calculated over the entire test dataset
calculate.deviation.intakerate <- function(RF.model.table) {
  observed <- colSums(RF.model.table)
  predicted <- rowSums(RF.model.table)
  intake.rate.obs <- observed["for-intake"]/(observed["for-search"]+observed["for-handle"]+observed["for-intake"])
  intake.rate.pred <- predicted["for-intake"]/(predicted["for-search"]+predicted["for-handle"]+predicted["for-intake"])
  ratio.pred.obs.intakerate <- intake.rate.pred/intake.rate.obs
  ratio.pred.obs.intakerate
}

# Functions for data visualisation
### Functions to calculate means and quantiles on matrices and arrays
quantile.0.025 <- function(x) quantile(na.omit(x), 0.025)
quantile.0.975 <- function(x) quantile(na.omit(x), 0.975)
calculate.F.measure <- function(sensitivity, precision) 2/(1/(sensitivity+0.001)+1/(precision+0.001)) # 0.001 is added to be able to still calculate an F measure when sensitivity or precision is 0. 

mean.naomit <- function(x) mean(na.omit(x)) # function to calculate mean after omitting NA values

# calculate mean and CRI over simulations
calculate.mean.CRI <- function(x) {
  mean.CRI <- array(NA, c(dim(x)[2],dim(x)[3],3), dimnames = list(dimnames(x)[[2]], dimnames(x)[[3]], c("mean","lcl","ucl")))
  mean.CRI[,,"mean"] <- apply(x,c(2,3),mean) # this excludes calculating the mean over part of the simulation that has a value. (for handle and drink) However, this gives very inaccurate estimates, sometimes based only on a few values.
  mean.CRI[,,"lcl"] <- apply(x,c(2,3),quantile.0.025)
  mean.CRI[,,"ucl"] <- apply(x,c(2,3),quantile.0.975)
  mean.CRI
}

# plot sensitivity, precision and F-measure for different behaviours as a function of (max) segment length or ARL value
plot.statistic.single <- function(x, xlabel="Segment length", statistic="Sensitivity", plot.x=T, plot.y=T, legend=F) { # x is a matrix with in columns what should be on the x-axis, and in rows the different behaviours
  xrange <- as.numeric(colnames(x))
  behaviours <- rownames(x)
  x <- x[behaviour.labels.ordered,] # order behaviours for plotting
  plot(c(min(xrange),max(xrange)), c(0,1), type="n", xlab="", ylab="", cex.lab=1.5, xaxt="n", yaxt="n")
  for (i in 1:dim(x)[1]) lines(xrange, x[i,], col=behaviour.colors[i])
  for (i in 1:dim(x)[1]) points(xrange, x[i,], pch=point.type[behaviour.labels==rownames(x)[i]], col=behaviour.colors[i], bg="white") # plot points after lines to avoid lines to cross over points
  if (legend==T) legend("bottomright", legend=behaviour.labels, pch=point.type)
  if (plot.x==T) {
    axis(1, at=xrange)
    mtext(xlabel, 1, 3, xpd=T)
  }
  if (plot.y==T) {
    axis(2, at=seq(0,1,0.2))
    mtext(statistic, 2, 3, xpd=T)
  }
}

plot.statistic.nsim.behaviour.x <- function(x, xadj=0, colour="grey") { # plot results as points to an existing plot, x is a matrix, with behavour in columns
  arrows(1:length(behaviour.pooled)+xadj, apply(x,2,mean), y1=apply(x,2,quantile.0.025), length=0)
  arrows(1:length(behaviour.pooled)+xadj, apply(x,2,mean), y1=apply(x,2,quantile.0.975), length=0)
  points(1:length(behaviour.pooled)+xadj, apply(x,2,mean), pch=21, bg=colour, cex=2)
}

plot.statistic.nsim.behaviour.symbol <- function(x, xlabel="Segment length", statistic="Sensitivity", x.numeric=T, plot.x=T, plot.y=T, xlas=1, xline=3, legend=F) { # x is an array with the first dimension being the behaviour, the 2nd what should be on the x-axis (segment length or ARL value), and the 3rd being the mean, lcl and ucl
  x <- x[behaviour.labels.ordered,,] # order behaviours for plotting
  if (x.numeric==T) xrange <- as.numeric(dimnames(x)[[2]]) else xrange <- 1:dim(x)[2]
  plot(xrange, x[1,,"mean"], xlim=c(min(xrange)-0.2*(max(xrange)-min(xrange))/length(xrange), max(xrange)+0.2*(max(xrange)-min(xrange))/length(xrange)), ylim=c(0,1), xaxt="n", yaxt="n", pch=19, type="n", xlab="", ylab="", las=1)
  for (i in 1:dim(x)[1]) {
    lines(xrange-(5-i)*0.05, x[i,,"mean"], col=behaviour.colors[i])
    arrows(xrange-(5-i)*0.05, x[i,,"mean"], y1=x[i,,"lcl"], length=0, col=behaviour.colors[i])
    arrows(xrange-(5-i)*0.05, x[i,,"mean"], y1=x[i,,"ucl"], length=0, col=behaviour.colors[i])
  }
  for (i in 1:dim(x)[1]) points(xrange-(5-i)*0.05, x[i,,"mean"], pch=point.type[i], bg="white", cex=1.3, col=behaviour.colors[i]) # plot the points after the lines, so that lines are not plotted over the points
  if (legend==T) legend("bottomright", legend=behaviour.labels.ordered, pch=point.type, col=behaviour.colors)
  if (plot.x==T) {
    if (x.numeric==T) axis(1, at=xrange, las=xlas) else axis(1, at=xrange, labels=dimnames(x)[[2]], las=xlas)
    mtext(xlabel, 1, xline, xpd=T) 
  }
  if (plot.y==T) {
    axis(2, at=seq(0,1,0.2), las=1)
    mtext(statistic, 2, 3.5, xpd=T)
  }
}

plot.CI.deviation.intakerate <- function(x, at=1, colour="grey") { # plot results as points to an existing plot, x is a vector
  arrows(at, mean(x), y1 = quantile.0.025(x), length=0)
  arrows(at, mean(x), y1 = quantile.0.975(x), length=0)
  points(at, mean(x), pch=21, bg=colour, cex=2)
}

calculate.prop.other.behaviours <- function(x, behaviour, class.error="FN") {
  if (class.error=="FN") other.behaviours <- rowSums(x[,behaviour,]) else other.behaviours <- rowSums(x[behaviour,,])
  other.behaviours <- other.behaviours[!(names(other.behaviours) %in% behaviour)]
  other.behaviours <- round(other.behaviours/sum(other.behaviours),4)
  other.behaviours
}

calculate.proportions <- function(x) x/sum(x) 

all.NA <- function(x) all(is.na(x))

link.gps.acc.data <- function(gps.data, acc.data, device.info) {
  data <- merge(gps.data, acc.data)
  data$x <- (data$x_acceleration-device.info$x_o)/device.info$x_s
  data$y <- (data$y_acceleration-device.info$y_o)/device.info$y_s
  data$z <- (data$z_acceleration-device.info$z_o)/device.info$z_s
  data <- na.omit(data[,c('device_info_serial','index','date_time',"longitude","latitude","altitude",'speed_2d','x','y','z')])
  data
}

predict.behaviour <- function(data, RF.model.fit, segmentation.method="fixed", segment.length=0.4, ARL.value=100) {
  data$obs.id <- paste(data$BirdID, as.numeric(data$date_time), sep = ".")
  # cut acc data into fixed or flexible segment lengths, with associated segment length or ARL value (depending on the RF model used):
  if (segmentation.method=="fixed") seg.df <- create.fixed.segments(segment.length, data, annotated.data=F, naomit=F) else seg.df <- create.flexible.segments(ARL.value, data, annotated.data=F, naomit=F)
  
  seg.df[[1]]$pred.behav <- predict(RF.model.fit, seg.df[[1]])
  data.new <- merge(unique(seg.df[[2]][,c("BirdID","date_time","altitude","latitude","longitude","speed_2d","segment.id.cut")]), seg.df[[1]])
  data.new
}

from.list.to.df <- function(list) {   # change from list to dataframe to have all birds together
  df <- list[[1]]
  for (i in 2:length(list)) df <- rbind(df, list[[i]])
  df
}

calculate.mean.95CI.from.logits <- function(df) {
  df$freq <- 1
  for.suc.min <- min(df$for.suc[df$for.suc>0])
  for.suc.max <- min(df$for.suc[df$for.suc<1])
  df$for.suc.for.logit <- df$for.suc
  df$for.suc.for.logit[df$for.suc==0] <- for.suc.min
  df$for.suc.for.logit[df$for.suc==1] <- for.suc.max
  df.mean.sd <- ddply(df, .(yday), summarize, 
                      N = sum(freq), for.suc.mean.logit = mean(qlogis(for.suc.for.logit)), for.suc.logit.sd = sd(qlogis(for.suc.for.logit)))
  df.mean.sd$se.logit <- df.mean.sd$for.suc.logit.sd/sqrt(df.mean.sd$N)
  df.mean.sd$li.logit <- df.mean.sd$for.suc.mean.logit - 1.96*df.mean.sd$se.logit # is the same as liw when calculated on the probability scale
  df.mean.sd$ui.logit <- df.mean.sd$for.suc.mean.logit + 1.96*df.mean.sd$se.logit # is the same as liw when calculated on the probability scale
  df.mean.sd$mean <- plogis(df.mean.sd$for.suc.mean.logit)
  df.mean.sd$li <- plogis(df.mean.sd$li.logit)
  df.mean.sd$ui <- plogis(df.mean.sd$ui.logit)
  df.mean.sd
}

add.columns.to.application.data <- function(df) {
  # link df with predicted behaviours per segment to df with habitat per date_time/BirdID
  df <- merge(df[df$latitude>53,c(names(df)[1:8],"pred.behav","mean.x","min.x","max.x","odba.x","odba.z","odba","dps.x","fdps.x","max.z")], df.uni.53[,c("BirdID","date_time","Name","habitat")], by=c("BirdID","date_time"))
  
  # remove 763 from the data:
  df <- df[df$BirdID!="763_2016",]
  
  # define foraging and ingesting prey
  df$foraging <- 0
  df$foraging[df$pred.behav %in% c("for-search","for-handle","for-intake")] <- 1
  df$ingest <- 0
  df$ingest[df$pred.behav == "for-intake"] <- 1
  # number of samples defined as foraging or ingest:
  df$nobs.foraging <- df$nobs.segments * df$foraging
  df$nobs.ingest <- df$nobs.segments * df$ingest
  
  # columns for year, month and yday:
  df$year <- year(df$date_time)
  df$month <- month(df$date_time)
  df$yday <- yday(df$date_time)
  df$hour <- hour(df$date_time)
  
  # add sex to the data
  bird.data$BirdID <- paste(bird.data$logger, bird.data$year.start, sep="_")
  df <- merge(df, bird.data[,c("BirdID","sex")])
  df <- df[order(df$BirdID, df$date_time),]
  
  # add some more columns
  df$land_water <- ifelse(df$habitat%in%c("LM_zoet","Noordzee","Schier_brak","Schier_Zoet","waddenzee","wal_rest_zoet"),"water","land")
  df$behav.pooled <- as.character(df$pred.behav)
  df$behav.pooled[df$pred.behav%in%c("fly-soar","fly-flap")] <- "fly"
  df$behav.pooled[df$pred.behav%in%c("sit","stand")] <- "rest"
  df$wadden <- ifelse(is.na(df$Name)==T,0,1)
  df$wadden2 <- ifelse(df$habitat=="waddenzee",1,0)
  
  df
}

calculate.ingest.rate <- function(x) x[,'for-intake'] / rowSums(x[,c('for-handle','for-search','for-intake')])
