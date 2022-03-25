## explore pattern of intakes to add a potentially useful summary statistic for this
behaviour.colors <- rainbow(max(acc.annotated$behaviour.index))
windows()
layout(matrix(1:16, ncol=4))
par(mar=c(1,1,1,1), oma=c(0,0,0,0))
for (i in unique(acc.annotated$segment.id[acc.annotated$behaviour.pooled=="for-intake"])[1:16]) {
  plot(x~Index, data=acc.annotated[acc.annotated$segment.id==i,], type="n", ylim=c(-1,1.5))
  lines(rep(acc.annotated$Index[acc.annotated$segment.id==i&acc.annotated$behaviour.pooled=="for-intake"],each=2),rep(c(-3,3),times=length(acc.annotated$Index[acc.annotated$segment.id==i&acc.annotated$behaviour.pooled=="for-intake"])), col="orange", lwd=2)
  points(x~Index, data=acc.annotated[acc.annotated$segment.id==i,], col="red", pch=19, cex=0.5)
  lines(x~Index, data=acc.annotated[acc.annotated$segment.id==i,], col="red")
  points(y~Index, data=acc.annotated[acc.annotated$segment.id==i,], col="blue", pch=19, cex=0.5)
  lines(y~Index, data=acc.annotated[acc.annotated$segment.id==i,], col="blue")
  points(z~Index, data=acc.annotated[acc.annotated$segment.id==i,], col="green", pch=19, cex=0.5)
  lines(z~Index, data=acc.annotated[acc.annotated$segment.id==i,], col="green")
}

# should we add correlations between the three axes to distinguish a prey intake? 
windows()
layout(matrix(1:16, ncol=4))
par(mar=c(1,1,1,1), oma=c(0,0,0,0))
for (i in unique(acc.annotated$segment.id[acc.annotated$behaviour.pooled=="for-intake"])[1:16]) {
  model <- lm(x~z, acc.annotated[acc.annotated$behaviour.pooled=="for-intake"&acc.annotated$segment.id==i,])
  pvalue <- summary(model)$coefficients[2,4]
  plot(x~z, acc.annotated[acc.annotated$behaviour.pooled=="for-intake"&acc.annotated$segment.id==i,])
  if (pvalue<0.05) abline(model)
}
# no, there seems no obvious correlations between the axes during a prey intake

# Run full and reduced RF model with segment length of 0.4 sec
dfs <- create.fixed.segments(0.4)
dfs[[1]]$diff.x <- dfs[[1]]$max.x-dfs[[1]]$min.x # I checked this variable as it appeared to be a potential candidate to distinguish prey intakes
dfs[[1]]$diff.y <- dfs[[1]]$max.y-dfs[[1]]$min.y
dfs[[1]]$diff.z <- dfs[[1]]$max.z-dfs[[1]]$min.z
names(dfs[[1]])
# test for correlation between predictors/features/summary statistics
correlationMatrix <- cor(dfs[[1]][,c(4:35,38:40)])
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
correlationMatrix[highlyCorrelated,] # odba and sd of the different axes are extremely correlation (0.991-0.997), so remove sd's from model. Also diff.x/y/z are strongly correlated with odba.x/y/z, so not used in the models. All predictors correlated <0.99 are kept in the models. 

# run different models with 10 simulations to derive CI's
nsim <- 10 

# first the model without downsampling with and without reducing the number of predictor variables
sensitivity.full.reduced.nsim.all <- array(NA, c(nsim,9,2), dimnames = list(1:nsim, behaviour.labels, c("full","reduced")))
precision.full.reduced.nsim.all <-array(NA, c(nsim,9,2), dimnames = list(1:nsim, behaviour.labels, c("full","reduced")))
deviation.intakerate.full.reduced.nsim.all <- matrix(nrow=nsim,ncol=2, dimnames = list(1:nsim, c("full","reduced")))

# use all annotated data to find the predictors that result in a decrease in accuracy of >20% when removed
RF.model.full.reduced.all.start <- RF.model.start(dfs[[1]], stand=1, search=1, drink=1, handle=1, walk=1, soar=1) # deze code werkt niet
# find the most important variables
RF.model.full.reduced.all.start.varImp <- randomForest::importance(RF.model.full.reduced.all.start)
predictors.all <- rownames(RF.model.full.reduced.all.start.varImp)
RF.model.full.reduced.all.start.varImp <- RF.model.full.reduced.all.start.varImp[order(RF.model.full.reduced.all.start.varImp[,"MeanDecreaseAccuracy"], decreasing=T),]
predictors.selected <- rownames(RF.model.full.reduced.all.start.varImp)[RF.model.full.reduced.all.start.varImp[,"MeanDecreaseAccuracy"]>20]

for (i in 1:nsim) { 
  for (j in 1:2) {
    if (j==1) RF.model.output <- RF.model(dfs[[1]], dfs[[2]], selected.variables = predictors.all, stand=1, search=1, drink=1, handle=1, walk=1, soar=1)[[2]]
    if (j==2) RF.model.output <- RF.model(dfs[[1]], dfs[[2]], selected.variables = predictors.selected, stand=1, search=1, drink=1, handle=1, walk=1, soar=1)[[2]]
    sensitivity.full.reduced.nsim.all[i,,j] <- calculate.performance(RF.model.output)[[1]]
    precision.full.reduced.nsim.all[i,,j] <- calculate.performance(RF.model.output)[[3]]
    deviation.intakerate.full.reduced.nsim.all[i,j] <- calculate.deviation.intakerate(RF.model.output)
    print(sensitivity.full.reduced.nsim.all)
  }
}
