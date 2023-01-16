# compare models with different levels of down vs upsampling
nsim=10
dfs <- create.fixed.segments(0.4) 

# downsampling only stand 4x, using all predictor variables (as reducing the nr of predictors doesn't really improve model performance)
sensitivity.stand4.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
precision.stand4.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
deviation.intakerate.stand4.nsim <- rep(NA,nsim)
confusion.mat.stand4.nsim <- array(NA, c(length(behaviour.pooled),length(behaviour.pooled), nsim), dimnames = list(behaviour.pooled, behaviour.pooled, 1:nsim))

for (i in 1:nsim) {
  RF.model.output <- RF.model(dfs[[1]], dfs[[2]], stand=4)
  confusion.mat.stand4.nsim[,,i] <- RF.model.output[[2]]
  performance <- calculate.performance(RF.model.output[[2]])
  sensitivity.stand4.nsim[i,] <- performance[[1]]
  precision.stand4.nsim[i,] <- performance[[3]]
  deviation.intakerate.stand4.nsim[i] <- calculate.deviation.intakerate(RF.model.output[[2]])
  print(sensitivity.stand4.nsim)
}

# now downsampling stand 4x and search 5x
sensitivity.stand4.search5.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
precision.stand4.search5.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
deviation.intakerate.stand4.search5.nsim <- rep(NA,nsim)
confusion.mat.stand4.search5.nsim <- array(NA, c(length(behaviour.pooled),length(behaviour.pooled), nsim), dimnames = list(behaviour.pooled, behaviour.pooled, 1:nsim))

for (i in 1:nsim) {
  RF.model.output <- RF.model(dfs[[1]], dfs[[2]], stand=4, search=5)
  confusion.mat.stand4.search5.nsim[,,i] <- RF.model.output[[2]]
  performance <- calculate.performance(RF.model.output[[2]])
  sensitivity.stand4.search5.nsim[i,] <- performance[[1]]
  precision.stand4.search5.nsim[i,] <- performance[[3]]
  deviation.intakerate.stand4.search5.nsim[i] <- calculate.deviation.intakerate(RF.model.output[[2]])
  print(sensitivity.stand4.search5.nsim)
}

# now downsampling stand 4x and search 2x
sensitivity.stand4.search2.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
precision.stand4.search2.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
deviation.intakerate.stand4.search2.nsim <- rep(NA,nsim)
confusion.mat.stand4.search2.nsim <- array(NA, c(length(behaviour.pooled),length(behaviour.pooled), nsim), dimnames = list(behaviour.pooled, behaviour.pooled, 1:nsim))

for (i in 1:nsim) {
  RF.model.output <- RF.model(dfs[[1]], dfs[[2]], stand=4, search=2)
  confusion.mat.stand4.search2.nsim[,,i] <- RF.model.output[[2]]
  performance <- calculate.performance(RF.model.output[[2]])
  sensitivity.stand4.search2.nsim[i,] <- performance[[1]]
  precision.stand4.search2.nsim[i,] <- performance[[3]]
  deviation.intakerate.stand4.search2.nsim[i] <- calculate.deviation.intakerate(RF.model.output[[2]])
  print(i)
  print(sensitivity.stand4.search2.nsim[i,])
  print(precision.stand4.search2.nsim[i,])
}
apply(calculate.F.measure(sensitivity.stand4.search2.nsim,precision.stand4.search2.nsim),2,mean)
apply(calculate.F.measure(sensitivity.stand4.search2.nsim,precision.stand4.search2.nsim),2,quantile.0.025)
apply(calculate.F.measure(sensitivity.stand4.search2.nsim,precision.stand4.search2.nsim),2,quantile.0.975)

# there seems to be a balance between searching and walking, perhaps it helps if we do not downsample searching, but upsample walking 10x (to become equally present in the dataset as searching):
sensitivity.walk10.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
precision.walk10.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
deviation.intakerate.walk10.nsim <- rep(NA,nsim)
confusion.mat.walk10.nsim <- array(NA, c(length(behaviour.pooled),length(behaviour.pooled), nsim), dimnames = list(behaviour.pooled, behaviour.pooled, 1:nsim))

for (i in 1:nsim) {
  RF.model.output <- RF.model(dfs[[1]], dfs[[2]], walk=10)
  confusion.mat.walk10.nsim[,,i] <- RF.model.output[[2]]
  performance <- calculate.performance(RF.model.output[[2]])
  sensitivity.walk10.nsim[i,] <- performance[[1]]
  precision.walk10.nsim[i,] <- performance[[3]]
  deviation.intakerate.walk10.nsim[i] <- calculate.deviation.intakerate(RF.model.output[[2]])
  print(sensitivity.walk10.nsim)
}

# what if we additionally upsample ingest
sensitivity.walk10.ingest6.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
precision.walk10.ingest6.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
deviation.intakerate.walk10.ingest6.nsim <- rep(NA,nsim)
confusion.mat.walk10.ingest6.nsim <- array(NA, c(length(behaviour.pooled),length(behaviour.pooled), nsim), dimnames = list(behaviour.pooled, behaviour.pooled, 1:nsim))

for (i in 1:nsim) {
  RF.model.output <- RF.model(dfs[[1]], dfs[[2]], walk=10, ingest=6)
  confusion.mat.walk10.ingest6.nsim[,,i] <- RF.model.output[[2]]
  performance <- calculate.performance(RF.model.output[[2]])
  sensitivity.walk10.ingest6.nsim[i,] <- performance[[1]]
  precision.walk10.ingest6.nsim[i,] <- performance[[3]]
  deviation.intakerate.walk10.ingest6.nsim[i] <- calculate.deviation.intakerate(RF.model.output[[2]])
  print(sensitivity.walk10.ingest6.nsim)
}

# now search 2x down, and ingest 6x and walk 10x up
sensitivity.search2.walk10.ingest6.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
precision.search2.walk10.ingest6.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
deviation.intakerate.search2.walk10.ingest6.nsim <- rep(NA,nsim)
confusion.mat.search2.walk10.ingest6.nsim <- array(NA, c(length(behaviour.pooled),length(behaviour.pooled), nsim), dimnames = list(behaviour.pooled, behaviour.pooled, 1:nsim))

for (i in 1:nsim) {
  RF.model.output <- RF.model(dfs[[1]], dfs[[2]], search=2, walk=10, ingest=6)
  confusion.mat.search2.walk10.ingest6.nsim[,,i] <- RF.model.output[[2]]
  performance <- calculate.performance(RF.model.output[[2]])
  sensitivity.search2.walk10.ingest6.nsim[i,] <- performance[[1]]
  precision.search2.walk10.ingest6.nsim[i,] <- performance[[3]]
  deviation.intakerate.search2.walk10.ingest6.nsim[i] <- calculate.deviation.intakerate(RF.model.output[[2]])
  print(sensitivity.search2.walk10.ingest6.nsim)
}

## choosing the most preferred option of the above (search 2x down, stand 4x down), now test how upsampling of other behaviours affects the sensitivity of these behaviours
sensitivity.stand4.search2.drink5.handle5.walk2.soar10.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
precision.stand4.search2.drink5.handle5.walk2.soar10.nsim <- matrix(NA, nrow=nsim, ncol=length(behaviour.pooled), dimnames = list(1:nsim, behaviour.pooled))
deviation.intakerate.stand4.search2.drink5.handle5.walk2.soar10.nsim <- rep(NA,nsim)
confusion.mat.stand4.search2.drink5.handle5.walk2.soar10.nsim <- array(NA, c(length(behaviour.pooled),length(behaviour.pooled), nsim), dimnames = list(behaviour.pooled, behaviour.pooled, 1:nsim))

for (i in 1:nsim) {
  RF.model.output <- RF.model(dfs[[1]], dfs[[2]], stand=4, search=2, drink=5, handle=5, walk=2, fly_passive=10)
  confusion.mat.stand4.search2.drink5.handle5.walk2.soar10.nsim[,,i] <- RF.model.output[[2]]
  performance <- calculate.performance(RF.model.output[[2]])
  sensitivity.stand4.search2.drink5.handle5.walk2.soar10.nsim[i,] <- performance[[1]]
  precision.stand4.search2.drink5.handle5.walk2.soar10.nsim[i,] <- performance[[3]]
  deviation.intakerate.stand4.search2.drink5.handle5.walk2.soar10.nsim[i] <- calculate.deviation.intakerate(RF.model.output[[2]])
  print(sensitivity.stand4.search2.drink5.handle5.walk2.soar10.nsim)
}