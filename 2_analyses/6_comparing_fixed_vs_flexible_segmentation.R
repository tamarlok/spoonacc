# run the fixed segmentation models with different segment lengths 100 times, for a selected number of fixed segments: 0.2, 0.4, 0.6, 0.8, 1, 1.5, and 2
nsim <- 100
seg.lengths <- c(0.2,0.4,0.6,0.8,1.0,1.5,2.0) # segment length in seconds
sensitivity.fixed.seglength.nsim <- array(NA, c(nsim,length(behaviour.pooled),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled, seg.lengths))
precision.fixed.seglength.nsim <- array(NA, c(nsim,length(behaviour.pooled),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled, seg.lengths))
deviation.intakerate.fixed.seglength.nsim <- matrix(nrow=nsim,ncol=length(seg.lengths), dimnames=list(1:nsim, seg.lengths))

df1.seg.length <- list(length(seg.lengths))
df2.seg.length <- list(length(seg.lengths))

for (j in 1:length(seg.lengths)) { 
  dfs <- create.fixed.segments(seg.lengths[j])
  df1.seg.length[[j]] <- dfs[[1]]
  df2.seg.length[[j]] <- dfs[[2]]
}

for (j in 1:length(seg.lengths)) { 
  print(seg.lengths[j])
  for (i in 1:nsim) {
    RF.model.output <- RF.model(df1.seg.length[[j]], df2.seg.length[[j]], stand=4)
    performance.stats <- calculate.performance(RF.model.output[[2]])
    sensitivity.fixed.seglength.nsim[i,,j] <- performance.stats[[1]]
    precision.fixed.seglength.nsim[i,,j] <- performance.stats[[3]]
    deviation.intakerate.fixed.seglength.nsim[i,j] <- calculate.deviation.intakerate(RF.model.output[[2]])
    print(i)
    print(precision.fixed.seglength.nsim[i,,j])
  }
}

# run the flexible segmentation models with different ARL values and a maximum sampling duration of 1.6 s
ARL.values <- c(100,500,5000,50000) # according to the manual of the cpm package, providing values other than the pre-defined values of 370, 500, 600, 700, ..., 1000, 2000, 3000, ..., 10000, 20000, ..., 50000 is not possible. However, the model does not give an error when using ARL0=100,200,370,400, but does when trying ARL0=50 or 350 for example.    
sensitivity.flexible.ARL.nsim <- array(NA, c(nsim,length(behaviour.pooled),length(ARL.values)), dimnames = list(1:nsim, behaviour.pooled, ARL.values))
precision.flexible.ARL.nsim <- array(NA, c(nsim,length(behaviour.pooled),length(ARL.values)), dimnames = list(1:nsim, behaviour.pooled, ARL.values))
deviation.intakerate.flexible.ARL.nsim <- matrix(nrow=nsim,ncol=length(ARL.values), dimnames=list(1:nsim, ARL.values))
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
    precision.flexible.ARL.nsim[i,,j] <- calculate.performance(RF.model.output[[2]])[[3]]
    deviation.intakerate.flexible.ARL.nsim[i,j] <- calculate.deviation.intakerate(RF.model.output[[2]])
    print(i)
    print(sensitivity.flexible.ARL.nsim[i,,j])
  }
}

# check samples sizes in training data for prey ingestions when using different segment lengths
for (i in 2:length(seg.lengths)) print(round(table(df1.seg.length[[i]]$behaviour.pooled)/table(df1.seg.length[[1]]$behaviour.pooled),2))
# proportional larger decrease in sample size for 'ingest' compared to other behaviours. Check if the decreasing sample size causes the decreasing classification performance for ingest:
# re-do one run of the RF model for each segment length with reducing the sample size of the dataset to the level of that at seg.length=2 s.
samples.to.keep.per.behaviour <- table(df1.seg.length[[7]]$behaviour.pooled)
sum(samples.to.keep.per.behaviour)

nsim=10
sensitivity.fixed.seglength.nsim.small.sample <- array(NA, c(nsim,length(behaviour.pooled),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled, seg.lengths))
precision.fixed.seglength.nsim.small.sample <- array(NA, c(nsim,length(behaviour.pooled),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled, seg.lengths))

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
    precision.fixed.seglength.nsim.small.sample[i,,j] <- performance.stats[[3]]
    print(seg.lengths[j])
    print(rbind(performance.stats[[1]], performance.stats[[2]], performance.stats[[3]]))
  }
}


# run the fixed.0.4 and flexible.100 models 100 times, and save confusion matrices:
nsim <- 100
dfs.fixed.0.4 <- list(df1.seg.length[[2]], df2.seg.length[[2]])
dfs.flex.100 <- list(df1.ARL.values[[1]], df2.ARL.values[[1]])
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