# Test model performance for unclean (all) vs clean segments (i.e. consisting of a single behaviour) :

### Run the model for different segment lengths
seg.lengths <- c(0.2,0.4,0.6,1.0,1.5,2.0) # segment length in seconds
sensitivity.all <- matrix(nrow=length(seg.lengths), ncol=length(behaviour.labels), dimnames=list(seg.lengths, behaviour.labels))
precision.all <- matrix(nrow=length(seg.lengths),ncol=length(behaviour.labels), dimnames=list(seg.lengths, behaviour.labels))
sensitivity.clean <- matrix(nrow=length(seg.lengths),ncol=length(behaviour.labels), dimnames=list(seg.lengths, behaviour.labels))
precision.clean <- matrix(nrow=length(seg.lengths),ncol=length(behaviour.labels), dimnames=list(seg.lengths, behaviour.labels))

for (i in 1:length(seg.lengths)) { 
  dfs <- create.fixed.segments(seg.lengths[i])
  # run model and calculate performance using also mixed-behaviour segments for training
  RF.model.output <- RF.model(dfs[[1]], dfs[[2]])
  performance.stats <- calculate.performance(RF.model.output[[2]])
  sensitivity.all[i,] <- performance.stats[[1]]
  precision.all[i,] <- performance.stats[[3]]
  print(sensitivity.all) # to follow the progress
  # run model and calculate performance using single-behaviour segments only for training
  RF.model.output <- RF.model(dfs[[1]], dfs[[2]], clean.segments.train=T)
  performance.stats <- calculate.performance(RF.model.output[[2]])
  sensitivity.clean[i,] <- performance.stats[[1]]
  precision.clean[i,] <- performance.stats[[3]]
  print(sensitivity.clean) # to follow the progress
}

calculate.F.measure(sensitivity.all, precision.all)
calculate.F.measure(sensitivity.clean, precision.clean)