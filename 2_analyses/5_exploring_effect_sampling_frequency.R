# Investigate effect of sampling frequency on classification performance
# using a fixed segment length of 0.8 s, so that with 2 Hz there are still two samples, and SD can be calculated 
nsim=10
sampling.freqs <- c(2,5,10,20) # when using a segment length of 0.4 s, you need at least 5 Hz to get 2 points in order to calculate sd/odba
sensitivity.sampling.freq.nsim <- array(NA, c(nsim,length(behaviour.pooled),length(sampling.freqs)), dimnames = list(1:nsim, behaviour.pooled, sampling.freqs))
precision.sampling.freq.nsim <-array(NA, c(nsim,length(behaviour.pooled),length(sampling.freqs)), dimnames = list(1:nsim, behaviour.pooled, sampling.freqs))

sensitivity.pooled.behaviours.sampling.freq.nsim <- array(NA, c(nsim,4,length(sampling.freqs)), dimnames = list(1:nsim, c("fly-active","fly-passive","active.ground","rest"), sampling.freqs))
precision.pooled.behaviours.sampling.freq.nsim <-array(NA, c(nsim,4,length(sampling.freqs)), dimnames = list(1:nsim, c("fly-active","fly-passive","active.ground","rest"), sampling.freqs))

for (j in 1:length(sampling.freqs)) { 
  print(sampling.freqs[j])
  dfs <- create.fixed.segments(0.8, sampling.freq=sampling.freqs[j], naomit=F) 
  for (i in 1:nsim) {
    RF.model.output <- RF.model(dfs[[1]], dfs[[2]], stand=4)
    performance.stats <- calculate.performance(RF.model.output[[2]])
    sensitivity.sampling.freq.nsim[i,,j] <- performance.stats[[1]]
    precision.sampling.freq.nsim[i,,j] <- performance.stats[[3]]
    # now for the pooled behaviours:
    performance.stats <- calculate.performance.pooled.behaviours(RF.model.output[[2]])
    sensitivity.pooled.behaviours.sampling.freq.nsim[i,,j] <- performance.stats[[1]]
    precision.pooled.behaviours.sampling.freq.nsim[i,,j] <- performance.stats[[3]]
    print(i)
    print(sensitivity.pooled.behaviours.sampling.freq.nsim[i,,j])
  }
}