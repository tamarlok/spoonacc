# run the model on 10 different train and test datasets to derive CI's
nsim <- 10 

# first the model without downsampling with and without reducing the number of predictor variables
sensitivity.full.reduced.nsim.all <- array(NA, c(nsim,9,2), dimnames = list(1:nsim, behaviour.pooled, c("full","reduced")))
precision.full.reduced.nsim.all <-array(NA, c(nsim,9,2), dimnames = list(1:nsim, behaviour.pooled, c("full","reduced")))
deviation.intakerate.full.reduced.nsim.all <- matrix(nrow=nsim,ncol=2, dimnames = list(1:nsim, c("full","reduced")))

# use all annotated data to find the predictors that result in a decrease in accuracy of >20% when removed
RF.model.full.reduced.all.start <- RF.model.start(dfs[[1]], stand=1, search=1, drink=1, handle=1, walk=1, fly_passive=1)
# find the most important variables
RF.model.full.reduced.all.start.varImp <- randomForest::importance(RF.model.full.reduced.all.start)
predictors.all <- rownames(RF.model.full.reduced.all.start.varImp)
RF.model.full.reduced.all.start.varImp <- RF.model.full.reduced.all.start.varImp[order(RF.model.full.reduced.all.start.varImp[,"MeanDecreaseAccuracy"], decreasing=T),]
predictors.selected <- rownames(RF.model.full.reduced.all.start.varImp)[RF.model.full.reduced.all.start.varImp[,"MeanDecreaseAccuracy"]>20]

for (i in 1:nsim) { 
  for (j in 1:2) {
    if (j==1) RF.model.output <- RF.model(dfs[[1]], dfs[[2]], selected.variables = predictors.all, stand=1, search=1, drink=1, handle=1, walk=1, fly_passive=1)[[2]]
    if (j==2) RF.model.output <- RF.model(dfs[[1]], dfs[[2]], selected.variables = predictors.selected, stand=1, search=1, drink=1, handle=1, walk=1, fly_passive=1)[[2]]
    sensitivity.full.reduced.nsim.all[i,,j] <- calculate.performance(RF.model.output)[[1]]
    precision.full.reduced.nsim.all[i,,j] <- calculate.performance(RF.model.output)[[3]]
    deviation.intakerate.full.reduced.nsim.all[i,j] <- calculate.deviation.intakerate(RF.model.output)
    print(sensitivity.full.reduced.nsim.all)
  }
}
