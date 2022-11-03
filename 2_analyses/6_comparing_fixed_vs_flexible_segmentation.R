# run the fixed segmentation models with different segment lengths 10 times (later increase this to 100x), for a selected number of fixed segments: 0.2, 0.4, 0.6, 0.8, 1, 1.5, and 2
nsim <- 100
seg.lengths <- c(0.2,0.4,0.6,0.8,1.0,1.5,2.0) # segment length in seconds
sensitivity.fixed.seglength.nsim <- array(NA, c(nsim,length(behaviour.labels),length(seg.lengths)), dimnames = list(1:nsim, behaviour.labels, seg.lengths))
specificity.fixed.seglength.nsim <- array(NA, c(nsim,length(behaviour.labels),length(seg.lengths)), dimnames = list(1:nsim, behaviour.labels, seg.lengths))
precision.fixed.seglength.nsim <- array(NA, c(nsim,length(behaviour.labels),length(seg.lengths)), dimnames = list(1:nsim, behaviour.labels, seg.lengths))
df1.seg.length <- list(length(seg.lengths))
df2.seg.length <- list(length(seg.lengths))

for (j in 1:length(seg.lengths)) { 
  dfs <- create.fixed.segments(seg.lengths[j])
  df1.seg.length[[j]] <- dfs[[1]]
  df2.seg.length[[j]] <- dfs[[2]]
}

for (j in 2:length(seg.lengths)) { 
  print(seg.lengths[j])
  for (i in 1:nsim) {
    RF.model.output <- RF.model(df1.seg.length[[j]], df2.seg.length[[j]], stand=4)
    performance.stats <- calculate.performance(RF.model.output[[2]])
    sensitivity.fixed.seglength.nsim[i,,j] <- performance.stats[[1]]
    specificity.fixed.seglength.nsim[i,,j] <- performance.stats[[2]]
    precision.fixed.seglength.nsim[i,,j] <- performance.stats[[3]]
    print(i)
    print(sensitivity.fixed.seglength.nsim[i,,j])
    print(specificity.fixed.seglength.nsim[i,,j])
    print(precision.fixed.seglength.nsim[i,,j])
  }
}

# run the flexible segmentation models with different ARL values and a maximum sampling duration of 1.6 s
ARL.values <- c(100,500,5000,50000) # according to the manual, providing values other than the pre-defined values of 370, 500, 600, 700, ..., 1000, 2000, 3000, ..., 10000, 20000, ..., 50000 is not possible. However, the model does not give an error when using ARL0=100,200,370,400, but does when trying ARL0=50 or 350 for example. This gives the error: "No thresholds available for selected ARL0, please see function documentation for supported values". This suggests that additional threshold values have now been added/calculated for ARL0=100,200,300,400 compared to previous versions. However, the distribution of segment lengths is very similar from ARL0=500 onward (but slighlty more short segments are made with ARL0=100).   
sensitivity.flexible.ARL.nsim <- array(NA, c(nsim,length(behaviour.labels),length(ARL.values)), dimnames = list(1:nsim, behaviour.labels, ARL.values))
specificity.flexible.ARL.nsim <- array(NA, c(nsim,length(behaviour.labels),length(ARL.values)), dimnames = list(1:nsim, behaviour.labels, ARL.values))
precision.flexible.ARL.nsim <- array(NA, c(nsim,length(behaviour.labels),length(ARL.values)), dimnames = list(1:nsim, behaviour.labels, ARL.values))
df1.ARL.values <- list(length(ARL.values))
df2.ARL.values <- list(length(ARL.values))

for (j in 1:length(ARL.values)) { 
  dfs <- create.flexible.segments(ARL.values[j], max.segment.length=1.6)
  df1.ARL.values[[j]] <- dfs[[1]]
  df2.ARL.values[[j]] <- dfs[[2]]
}

for (j in 1:length(ARL.values)) { 
  print(ARL.values[j])
  for (i in 1:nsim) {
    RF.model.output <- RF.model(df1.ARL.values[[j]], df2.ARL.values[[j]], stand=4)
    sensitivity.flexible.ARL.nsim[i,,j] <- calculate.performance(RF.model.output[[2]])[[1]]
    specificity.flexible.ARL.nsim[i,,j] <- calculate.performance(RF.model.output[[2]])[[2]]
    precision.flexible.ARL.nsim[i,,j] <- calculate.performance(RF.model.output[[2]])[[3]]
    print(i)
    print(sensitivity.flexible.ARL.nsim[i,,j])
    print(specificity.flexible.ARL.nsim[i,,j])
    print(precision.flexible.ARL.nsim[i,,j])
  }
}

# check samples sizes in training data for prey ingestions when using different segment lengths
for (i in 2:length(seg.lengths)) print(round(table(df1.seg.length[[i]]$behaviour.pooled)/table(df1.seg.length[[1]]$behaviour.pooled),2))
# proportional larger decrease in sample size for for-intake than for other behaviours. however, a decrease in sample size not necessarily means a decrease in performance. but it could be... 
# re-do one run of the RF model for each segment length with reducing the sample size of the dataset to the level of that at seg.length=2 s.
samples.to.keep.per.behaviour <- table(df1.seg.length[[7]]$behaviour.pooled)
sum(samples.to.keep.per.behaviour) # a total of 2545 segments

nsim=10
sensitivity.fixed.seglength.nsim.small.sample <- array(NA, c(nsim,length(behaviour.labels),length(seg.lengths)), dimnames = list(1:nsim, behaviour.labels, seg.lengths))
specificity.fixed.seglength.nsim.small.sample <- array(NA, c(nsim,length(behaviour.labels),length(seg.lengths)), dimnames = list(1:nsim, behaviour.labels, seg.lengths))
precision.fixed.seglength.nsim.small.sample <- array(NA, c(nsim,length(behaviour.labels),length(seg.lengths)), dimnames = list(1:nsim, behaviour.labels, seg.lengths))

for (j in 1:length(seg.lengths)) { 
  for (i in 1:nsim) {
    # available samples per behaviour:
    table(df1.seg.length[[j]]$behaviour.pooled)
    indices.to.keep <- NULL
    # reduce the dataset to the number in samples.to.keep.per.behaviour:
    for (k in unique(df1.seg.length[[j]]$behaviour.pooled)) {
      indices.behaviour <- which(df1.seg.length[[j]]$behaviour.pooled==k)
      indices.behaviour.to.keep <- sample(indices.behaviour, samples.to.keep.per.behaviour[names(samples.to.keep.per.behaviour)==k], replace=F)
      indices.to.keep <- c(indices.to.keep, indices.behaviour.to.keep)
    }
    length(indices.to.keep) # same as sum(samples.to.keep.per.behaviour)
    df1 <- df1.seg.length[[j]][indices.to.keep,]
    # segment names to keep:
    df2 <- df2.seg.length[[j]][df2.seg.length[[j]]$segment.id.cut%in%df1$segment.id.cut,]
    RF.model.output <- RF.model(df1, df2, stand=4)
    performance.stats <- calculate.performance(RF.model.output[[2]])
    sensitivity.fixed.seglength.nsim.small.sample[i,,j] <- performance.stats[[1]]
    specificity.fixed.seglength.nsim.small.sample[i,,j] <- performance.stats[[2]]
    precision.fixed.seglength.nsim.small.sample[i,,j] <- performance.stats[[3]]
    print(seg.lengths[j])
    print(rbind(performance.stats[[1]], performance.stats[[2]], performance.stats[[3]]))
  }
}


# run a single run of the best-supported fixed and flexible segmentation model, to get a confusion matrix for both. 
dfs.fixed.0.4 <- list(df1.seg.length[[2]], df2.seg.length[[2]])
dfs.flex.100 <- list(df1.ARL.values[[1]], df2.ARL.values[[1]])
RF.model.output.fixed <- RF.model(df1.seg.length[[2]], df2.seg.length[[2]], stand=4) # the 2nd segment length is 0.4 s
RF.model.output.flex <- RF.model(df1.ARL.values[[1]], df2.ARL.values[[1]], stand=4) # the first ARL value is 100. 
RF.model.output.fixed[[2]]
RF.model.output.flex[[2]]
# copy for Table S3
write.table(RF.model.output.fixed[[2]], "clipboard", sep="\t")
write.table(RF.model.output.flex[[2]], "clipboard", sep="\t")

# run the fixed.0.4 and flexible.100 models 100 times, and save confusion matrices:
nsim <- 100
confusion.matrix.fixed.0.4.nsim <- array(NA, c(nsim, dim(RF.model.output.fixed[[2]])), dimnames = list(1:nsim, colnames(RF.model.output.fixed[[2]]), rownames(RF.model.output.fixed[[2]])))
confusion.matrix.flex.100.nsim <- array(NA, c(nsim, dim(RF.model.output.flex[[2]])), dimnames = list(1:nsim, colnames(RF.model.output.flex[[2]]), rownames(RF.model.output.flex[[2]])))

for (i in 1:nsim) {
  print(i)
  # fixed 0.4
  RF.model.output <- RF.model(dfs.fixed.0.4[[1]], dfs.fixed.0.4[[2]], stand=4)
  confusion.matrix.fixed.0.4.nsim[i,,] <- RF.model.output[[2]]
  print(confusion.matrix.fixed.0.4.nsim[i,,])
  # flex 100
  RF.model.output <- RF.model(dfs.flex.100[[1]], dfs.flex.100[[2]], stand=4)
  confusion.matrix.flex.100.nsim[i,,] <- RF.model.output[[2]]
  print(confusion.matrix.flex.100.nsim[i,,])
}