## define point types used for different behaviours, in the order:
## sit, stand, fly-flap, fly-soar, forage-search, forage-handle, forage-intake, walk, drink
behaviour.pooled <- c("sit", "stand", "fly-flap", "fly-soar", "for-search", "for-handle", "for-intake", "walk", "drink")
behaviour.labels.ordered <- c("sit", "stand", "fly-flap", "fly-soar", "forage-search", "forage-handle", "forage-intake", "walk", "drink")
behaviour.labels.ordered.short <- c("sit", "stand", "fly (active)", "fly (passive)", "search", "handle", "ingest", "walk", "drink")
point.type <- c(24,25,23,22,24,23,25,21,22) 
behaviour.colors <- brewer.pal(length(behaviour.labels.ordered), "Paired")
# switch colors for search (orange), handle (pink) and intake (red):
behaviour.colors <- behaviour.colors[c(1:4,7,5,6,8:9)]

# distribution of segment length of the first flexiby-cut segment within the 1.6 s samples, showing that these segments can be as small as 3 observations:
windows()
hist(dfs.flex.100[[1]][substr(dfs.flex.100[[1]]$segment.id.cut,nchar(dfs.flex.100[[1]]$segment.id.cut),nchar(dfs.flex.100[[1]]$segment.id.cut))=="a",'nobs.segments'], main="", xlab="# observations in first flexibly cut segment")
# plotting the distribution of all flexibly cut segments:
hist(dfs.flex.100[[1]]$nobs.segments, main="", xlab="# observations of all flexibly cut segments")

# plot the distribution of segment lengths for the different ARL values. 
windows()
layout(matrix(1:8,ncol=2))
# j is the index for the different ARL values
hist(df1.ARL.values[[j]][substr(df1.ARL.values[[j]]$segment.id.cut,nchar(dfs.flex.100[[1]]$segment.id.cut),nchar(dfs.flex.100[[1]]$segment.id.cut))=="a",'nobs.segments'], main="", xlab="# observations in first flexibly cut segment")
# plotting the distribution of all flexibly cut segments:
hist(dfs.flex.100[[1]]$nobs.segments, main="", xlab="# observations of all flexibly cut segments")


# Effect of maximum segment length (only a single round/simulation of model training and validation)
F.flexible.ARL.maxlength <- calculate.F.measure(sensitivity.flexible.ARL.maxlength, precision.flexible.ARL.maxlength)
windows(10,10)
layout(matrix(1:6, ncol=2, byrow=F))
par(mar=c(1,1,0,0), oma=c(5,5,3,8), xpd=T)
plot.statistic.single(sensitivity.flexible.ARL.maxlength[1,behaviour.labels.ordered,], xlab="", statistic="Sensitivity", plot.x=F)
mtext("ARL=500",3,1,xpd=T)
plot.statistic.single(precision.flexible.ARL.maxlength[1,behaviour.labels.ordered,], xlab="", statistic="Precision", plot.x=F)
plot.statistic.single(F.flexible.ARL.maxlength[1,behaviour.labels.ordered,], xlab="Maximum segment length", statistic="F-measure")
plot.statistic.single(sensitivity.flexible.ARL.maxlength[2,behaviour.labels.ordered,], xlab="", statistic="Sensitivity", plot.x=F, plot.y=F)
mtext("ARL=5000",3,1,xpd=T)
plot.statistic.single(precision.flexible.ARL.maxlength[2,behaviour.labels.ordered,], xlab="", statistic="Precision", plot.x=F, plot.y=F)
plot.statistic.single(F.flexible.ARL.maxlength[2,behaviour.labels.ordered,], xlab="Maximum segment length", statistic="F-measure", plot.y=F)
legend("bottomright", inset=c(-0.45,0), legend=behaviour.labels.ordered.short, pch=point.type, xpd=NA, col=behaviour.colors)

# Effect startup value
F.effect.startup.value <- calculate.F.measure(sensitivity.effect.startup.value, precision.effect.startup.value)
windows(5,8)
layout(1:3)
par(mar=c(1,1,0,0), oma=c(4,4,3,8), xpd=T)
plot.statistic.single(t(sensitivity.effect.startup.value), xlabel="", statistic="Sensitivity", plot.x=F)
mtext("ARL=5000 and max seg length=1.6 s",3,1,xpd=T) # default ARL value used
plot.statistic.single(t(precision.effect.startup.value), xlabel="", statistic="Precision", plot.x=F)
plot.statistic.single(t(F.effect.startup.value), xlab="Startup value", statistic="F-measure")
legend("bottomright", inset=c(-0.45,0), legend=behaviour.labels.ordered.short, pch=point.type, xpd=NA, col=behaviour.colors)

#################
### FIGURE S5 ###
#################
pdf("output/FigS5.pdf",width=9,height=6)
# windows(9,6)
layout(1:3)
par(mar=c(1,5,0,1), oma=c(2,0,13,0))
# Sensitivity
plot(c(1,length(behaviour.labels)), c(0,1), xlim=c(1,9.5), ylim=c(0,1), xaxt="n", pch=21, xlab="", ylab="Sensitivity", cex=2, las=1, bg="grey", type="n", cex.axis=1.3, cex.lab=1.5)
plot.statistic.nsim.behaviour.x(sensitivity.full.reduced.nsim.all[,behaviour.labels.ordered,"full"], colour="blue", xadj=-0.2)
plot.statistic.nsim.behaviour.x(sensitivity.full.reduced.nsim.all[,behaviour.labels.ordered,"reduced"], colour="green", xadj=-0.1)
plot.statistic.nsim.behaviour.x(sensitivity.stand4.nsim[,behaviour.labels.ordered], colour="red", xadj=0)
plot.statistic.nsim.behaviour.x(sensitivity.walk10.nsim[,behaviour.labels.ordered], colour="lightblue", xadj=0.1)
plot.statistic.nsim.behaviour.x(sensitivity.walk10.ingest6.nsim[,behaviour.labels.ordered], colour="lavender", xadj=0.2)
plot.statistic.nsim.behaviour.x(sensitivity.stand4.search2.nsim[,behaviour.labels.ordered], colour="orange", xadj=0.3)
plot.statistic.nsim.behaviour.x(sensitivity.stand4.search2.drink5.handle5.walk2.soar10.nsim[,behaviour.labels.ordered], colour="purple", xadj=0.4)
plot.statistic.nsim.behaviour.x(sensitivity.search2.walk10.ingest6.nsim[,behaviour.labels.ordered], colour="lightcoral", xadj=0.5)
plot.statistic.nsim.behaviour.x(sensitivity.stand4.search5.nsim[,behaviour.labels.ordered], colour="pink", xadj=0.6)
legend("top", inset=c(-1,-1.35), legend=c("all predictors","only best predictors","DOWN stand 4x", "UP walk 10x", "UP walk 10x ingest 6x", "DOWN stand 4x, search 2x", "DOWN stand 4x, search 2x, UP drink 5x, handle 5x, walk 2x, soar 10x", "DOWN search 2x, UP walk 10x, ingest 6x", "DOWN stand 4x, search 5x"), pch=21, xpd=NA, pt.bg=c("blue","green","red","lightblue","lavender","orange","purple","lightcoral","pink"), cex=1.2, pt.cex=2)
# Precision
plot(c(1,length(behaviour.labels)), c(0,1), xlim=c(1,9.5), ylim=c(0,1), xaxt="n", pch=21, xlab="", ylab="Precision", cex=2, las=1, bg="grey", type="n", cex.axis=1.3, cex.lab=1.5)
plot.statistic.nsim.behaviour.x(precision.full.reduced.nsim.all[,behaviour.labels.ordered,"full"], colour="blue", xadj=-0.2)
plot.statistic.nsim.behaviour.x(precision.full.reduced.nsim.all[,behaviour.labels.ordered,"reduced"], colour="green", xadj=-0.1)
plot.statistic.nsim.behaviour.x(precision.stand4.nsim[,behaviour.labels.ordered], colour="red", xadj=0)
plot.statistic.nsim.behaviour.x(precision.walk10.nsim[,behaviour.labels.ordered], colour="lightblue", xadj=0.1)
plot.statistic.nsim.behaviour.x(precision.walk10.ingest6.nsim[,behaviour.labels.ordered], colour="lavender", xadj=0.2)
plot.statistic.nsim.behaviour.x(precision.stand4.search2.nsim[,behaviour.labels.ordered], colour="orange", xadj=0.3)
plot.statistic.nsim.behaviour.x(precision.stand4.search2.drink5.handle5.walk2.soar10.nsim[,behaviour.labels.ordered], colour="purple", xadj=0.4)
plot.statistic.nsim.behaviour.x(precision.search2.walk10.ingest6.nsim[,behaviour.labels.ordered], colour="lightcoral", xadj=0.5)
plot.statistic.nsim.behaviour.x(precision.stand4.search5.nsim[,behaviour.labels.ordered], colour="pink", xadj=0.6)
# F-measure
plot(c(1,length(behaviour.labels)), c(0,1), xlim=c(1,9.5), ylim=c(0,1), xaxt="n", pch=21, xlab="", ylab="F-measure", cex=2, las=1, bg="grey", type="n", cex.axis=1.3, cex.lab=1.5)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.full.reduced.nsim.all[,behaviour.labels.ordered,"full"], precision.full.reduced.nsim.all[,behaviour.labels.ordered,"full"]), colour="blue", xadj=-0.2)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.full.reduced.nsim.all[,behaviour.labels.ordered,"reduced"], precision.full.reduced.nsim.all[,behaviour.labels.ordered,"reduced"]), colour="green", xadj=-0.1)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.stand4.nsim[,behaviour.labels.ordered], precision.stand4.nsim[,behaviour.labels.ordered]), colour="red", xadj=0)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.walk10.nsim[,behaviour.labels.ordered], precision.walk10.nsim[,behaviour.labels.ordered]), colour="lightblue", xadj=0.1)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.walk10.ingest6.nsim[,behaviour.labels.ordered], precision.walk10.ingest6.nsim[,behaviour.labels.ordered]), colour="lavender", xadj=0.2)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.stand4.search2.nsim[,behaviour.labels.ordered], precision.stand4.search2.nsim[,behaviour.labels.ordered]), colour="orange", xadj=0.3)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.stand4.search2.drink5.handle5.walk2.soar10.nsim[,behaviour.labels.ordered], precision.stand4.search2.drink5.handle5.walk2.soar10.nsim[,behaviour.labels.ordered]), colour="purple", xadj=0.4)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.search2.walk10.ingest6.nsim[,behaviour.labels.ordered], precision.search2.walk10.ingest6.nsim[,behaviour.labels.ordered]), colour="lightcoral", xadj=0.5)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.stand4.search5.nsim[,behaviour.labels.ordered], precision.stand4.search5.nsim[,behaviour.labels.ordered]), colour="pink", xadj=0.6)
axis(1, at=1:9+0.2, labels=behaviour.labels.ordered.short, las=1, cex.axis=1.4)
dev.off()
### END FIGURE S5 ###

# compare the models stand4 and stand4.search5 and investigate with which behaviours search, ingest and walk are mostly confused. 
# depending on whether searching behaviour is downsampled 5x or not (standing was downsampled 4x in both cases)
# reported percentages may slightly differ from the estimates, as a new random run may have been performed to train and test/validate the model. 
# when searching is not downsampled:
calculate.prop.other.behaviours(confusion.mat.stand4.nsim, "walk", class.error="FN") # 90% of the FN walk was wrongly predicted as search
calculate.prop.other.behaviours(confusion.mat.stand4.nsim, "walk", class.error="FP") # 72% of the FP walk was actually search, 15% ingest 
calculate.prop.other.behaviours(confusion.mat.stand4.nsim, "forage-search", class.error="FN") # 65% of FN search was wrongly predicted as ingest; 25% as walk 
calculate.prop.other.behaviours(confusion.mat.stand4.nsim, "forage-search", class.error="FP") # 42% of FP search was actually ingest, 28% walking and 23% standing. 
calculate.prop.other.behaviours(confusion.mat.stand4.nsim, "forage-intake", class.error="FN") # 96% wrongly predicted as search
calculate.prop.other.behaviours(confusion.mat.stand4.nsim, "forage-intake", class.error="FP") # 69% was actually searching, 26% handling prey 
# when the amount of segments with stand as dominant behaviour was reduced 4 times, and 5 times for searching segments in the training dataset
calculate.prop.other.behaviours(confusion.mat.stand4.search5.nsim, "walk", class.error="FN") # 58% wrongly predicted as searching, 21% as ingesting prey and 20% as standing (this is very similar to what I got when downsampling was also done on the testing dataset, so it is not just caused by the fact that there is less searching behaviour in the test dataset)
calculate.prop.other.behaviours(confusion.mat.stand4.search5.nsim, "walk", class.error="FP") # 79% was actually search, 15% standing (this is quite different from the downsampled test dataset)
calculate.prop.other.behaviours(confusion.mat.stand4.search5.nsim, "forage-search", class.error="FN") # 55% wrongly predicted as prey ingestion; 38% as walk 
calculate.prop.other.behaviours(confusion.mat.stand4.search5.nsim, "forage-search", class.error="FP") # 54% was actually ingesting prey, 25% walking and 14% standing. 
calculate.prop.other.behaviours(confusion.mat.stand4.search5.nsim, "forage-intake", class.error="FN") # 87% wrongly predicted as search, 13% as walk
calculate.prop.other.behaviours(confusion.mat.stand4.search5.nsim, "forage-intake", class.error="FP") # 83% was actually searching, 12% handling prey 
calculate.prop.other.behaviours(confusion.mat.stand4.search5.nsim, "drink", class.error="FN") # drinking was equally often confused with searching, standing and walking. Somewhat surprisingly, it was never estimated as prey ingestion. I guess because the movement is much slower than when swallowing a prey. 

# effect of down- and upsampling on estimating prey intake rates:
windows()
plot(c(1,4), c(0.8,1.7), type="n", xlim=c(0.5,5.5), xaxt="n", xlab="", ylab="deviation of estimated intake rate")
plot.CI.deviation.intakerate(deviation.intakerate.stand4.nsim, at=1, colour="red")
plot.CI.deviation.intakerate(deviation.intakerate.stand4.search5.nsim, at=2, colour="pink")
plot.CI.deviation.intakerate(deviation.intakerate.stand4.search2.nsim, at=3, colour="orange")
plot.CI.deviation.intakerate(deviation.intakerate.walk10.ingest6.nsim, at=4, colour="lavender")
plot.CI.deviation.intakerate(deviation.intakerate.search2.walk10.ingest6.nsim, at=5, colour="lightcoral")
axis(1, at=1:5, labels=c("none","search -5x","search -2x", "walk 10x, ingest 6x", "search -2x"))
axis(1, at=1:5, line=1, labels=c("","","", "", "walk 10x, ingest 6x"), tick=F)
# the deviation in prey ingestion rates is depending on the proportion of foraging (searching and ingesting) behaviour in the test dataset. This should ideally be closely matching the proportion expressed in the wild. 
# additionally, it is questionable whether we should aim for a value close to 1, or a value with the smallest SE (taking for granted a persistent over- or underestimation as relative comparisons are still possible). 
# for both aims, it is good to choose the "none" option, i.e. no down- or upsampling of search, ingest or walk.

# investigate the effect of sampling frequency
# calculate mean, lcl and ucl over 10 simulations for all separate behaviours
mean.CRI.sensitivity.sampling.freq <- calculate.mean.CRI(sensitivity.sampling.freq.nsim)
mean.CRI.precision.sampling.freq <- calculate.mean.CRI(precision.sampling.freq.nsim)
F.measure.sampling.freq <- calculate.F.measure(sensitivity.sampling.freq.nsim, precision.sampling.freq.nsim)
mean.CRI.F.measure.sampling.freq <- calculate.mean.CRI(F.measure.sampling.freq)
### calculate mean, lcl and ucl over 10 simulations for pooled behaviours
mean.CRI.sensitivity.pooled.behaviours.sampling.freq <- calculate.mean.CRI(sensitivity.pooled.behaviours.sampling.freq.nsim)
mean.CRI.precision.pooled.behaviours.sampling.freq <- calculate.mean.CRI(precision.pooled.behaviours.sampling.freq.nsim)
F.measure.pooled.behaviours.sampling.freq <- calculate.F.measure(sensitivity.pooled.behaviours.sampling.freq.nsim, precision.pooled.behaviours.sampling.freq.nsim)
mean.CRI.F.measure.pooled.behaviours.sampling.freq <- calculate.mean.CRI(F.measure.pooled.behaviours.sampling.freq)

################
### FIGURE S11 ###
################
pdf("output/FigS11.pdf", width=4, height=6)
#windows(4,6) 
layout(matrix(1:3, ncol=1, byrow=F))
par(mar=c(1,1,0,10), oma=c(5,5,1,0), xpd=T)
plot.statistic.nsim.behaviour.symbol(mean.CRI.sensitivity.sampling.freq, xlabel="", statistic="Sensitivity", plot.x=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.precision.sampling.freq, xlabel="", statistic="Precision", plot.x=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.F.measure.sampling.freq, xlabel="Sampling frequency", statistic="F-measure", x.numeric=F)
legend("bottomright", inset=c(-0.6,0), legend=behaviour.labels.ordered.short, pch=point.type, xpd=NA, col=behaviour.colors)
dev.off()
### End Figure S11 ###

### graph with broad-scale behavours ###
behaviours.broad <- dimnames(mean.CRI.sensitivity.pooled.behaviours.sampling.freq)[[1]]
plot.statistic.nsim.behaviour.symbol(mean.CRI.sensitivity.pooled.behaviours.sampling.freq, xlabel="", statistic="", plot.x=F, x.numeric=F, behaviours=behaviours.broad, colors=behaviour.colors[c(3,4,8,2)])
plot.statistic.nsim.behaviour.symbol(mean.CRI.precision.pooled.behaviours.sampling.freq, xlabel="", statistic="", plot.x=F, x.numeric=F, behaviours=behaviours.broad, colors=behaviour.colors[c(3,4,8,2)])
plot.statistic.nsim.behaviour.symbol(mean.CRI.F.measure.pooled.behaviours.sampling.freq, xlabel="Sampling frequency", statistic="", x.numeric=F, behaviours=behaviours.broad, colors=behaviour.colors[c(3,4,8,2)])
legend("bottomright", inset=c(-0.8,0), legend=behaviours.broad, pch=point.type, xpd=NA, col=behaviour.colors[c(3,4,8,2)])

# comparison of model performance using fixed versus flexible segmentation method 

### calculate mean, lcl and ucl over 100 simulations
mean.CRI.sensitivity.fixed.seglength <- calculate.mean.CRI(sensitivity.fixed.seglength.nsim)
mean.CRI.precision.fixed.seglength <- calculate.mean.CRI(precision.fixed.seglength.nsim)
mean.CRI.specificity.fixed.seglength <- calculate.mean.CRI(specificity.fixed.seglength.nsim)
F.measure.fixed.seglength.nsim <- calculate.F.measure(sensitivity.fixed.seglength.nsim, precision.fixed.seglength.nsim)
mean.CRI.Fmeasure.fixed.seglength <- calculate.mean.CRI(F.measure.fixed.seglength.nsim)
mean.CRI.sensitivity.flexible.ARL <- calculate.mean.CRI(sensitivity.flexible.ARL.nsim)
mean.CRI.precision.flexible.ARL <- calculate.mean.CRI(precision.flexible.ARL.nsim)
mean.CRI.Fmeasure.flexible.ARL <- calculate.mean.CRI(calculate.F.measure(sensitivity.flexible.ARL.nsim, precision.flexible.ARL.nsim))
### calculate mean, lcl and ucl over 10 simulations using for each segment length the same sample size as for the longest segment length (2.0 s) (to remove the potential confounding effect of smaller sample sizes at long fixed segments for behaviours that last relatively short, such as ingesting prey). 
mean.CRI.sensitivity.fixed.seglength.small.sample <- calculate.mean.CRI(sensitivity.fixed.seglength.nsim.small.sample)
mean.CRI.precision.fixed.seglength.small.sample <- calculate.mean.CRI(precision.fixed.seglength.nsim.small.sample)
mean.CRI.Fmeasure.fixed.seglength.small.sample <- calculate.mean.CRI(calculate.F.measure(sensitivity.fixed.seglength.nsim.small.sample, precision.fixed.seglength.nsim.small.sample))
# compare F-measures for searching
mean.CRI.Fmeasure.fixed.seglength["forage-search",,] # CRI's are overlapping for 0.2-1.0 s 
mean.CRI.Fmeasure.flexible.ARL["forage-search",,] # CRI's are overlapping for ARL=100, 500 and 5000
# highest and almost identical at fixed seglength 0.4 s (0.91, 0.90-0.92) and ARL 100 (0.91, 0.90-0.92)
# compare F-measures for prey intakes
mean.CRI.Fmeasure.fixed.seglength["forage-intake",,] # CRI is overlapping for 0.4-0.8 s 
mean.CRI.Fmeasure.flexible.ARL["forage-intake",,] # CRI for ARL=100 and 500 are overlapping 
# highest and almost identical at fixed seglength 0.4 s (0.73, 0.70-0.76) and ARL 100 (0.73, 0.70-0.77)
mean.CRI.Fmeasure.fixed.seglength["walk",,]  
mean.CRI.Fmeasure.flexible.ARL["walk",,] 

################
### FIGURE 3 ###
################
pdf("output/Fig3.pdf",width=6,height=6)
#windows()
layout(matrix(c(1,1,2,1,1,2,1,1,2,1,1,2,
                3,3,4,3,3,4,3,3,4,3,3,4,
                5,5,6,5,5,6,5,5,6,5,5,6), ncol=3, byrow=T))
par(mar=c(1,1,0,0), oma=c(5,5,1,10), xpd=T)
plot.statistic.nsim.behaviour.symbol(mean.CRI.sensitivity.fixed.seglength, xlabel="", statistic="Sensitivity", plot.x=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.sensitivity.flexible.ARL, xlabel="", statistic="Sensitivity", plot.x=F, plot.y=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.precision.fixed.seglength, xlabel="", statistic="Precision", plot.x=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.precision.flexible.ARL, xlabel="", statistic="Precision", plot.x=F, plot.y=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.Fmeasure.fixed.seglength, xlabel="", statistic="F-measure", plot.x=T, x.numeric=F)
mtext("Fixed segment length (s)", 1, 2.5, padj=1)
plot.statistic.nsim.behaviour.symbol(mean.CRI.Fmeasure.flexible.ARL, xlabel="", statistic="F-measure", plot.x=T, plot.y=F, x.numeric=F, xlas=2)
mtext(expression(ARL[0]*" value"), 1, 2.5, padj=1)
legend("bottomright", inset=c(-.9,0), legend=behaviour.labels.ordered.short, pch=point.type, xpd=NA, col=behaviour.colors)
dev.off()
### END FIGURE 3 ###

# Do z-tests for F-measure per behaviour between the different segment lengths and ARL values
# save t- and p-values per behaviour and comparison
t.fixed <- matrix(nrow=length(behaviour.labels.ordered), ncol=6, dimnames=list(behaviour.labels.ordered, c("1-2","2-3","3-4","4-5","5-6","6-7")))
p.fixed <- matrix(nrow=length(behaviour.labels.ordered), ncol=6, dimnames=list(behaviour.labels.ordered, c("1-2","2-3","3-4","4-5","5-6","6-7")))
for (j in behaviour.labels.ordered) {
  df <- F.measure.fixed.seglength.nsim[,j,]
  prob.min <- min(df[df>0])
  prob.max <- max(df[df<1])
  df[df==0] <- prob.min
  df[df>=1] <- prob.max
  df.logit <- qlogis(df)
  for (i in 1:6) {
    if (length(na.omit(df.logit[,i]))>50&length(na.omit(df.logit[,i+1]))>50) {
      test.results <- t.test(df.logit[,i], df.logit[,i+1])
      t.fixed[j,i] <- round(test.results$statistic,3)
      p.fixed[j,i] <- round(test.results$p.value,4)
    }
  }
}
p.fixed
# these tests do not have so much value, as if you do enough simulations, even the subtlest difference will become significant. The question is however, how big a difference in F-measure is still meaningful/relevant. As described above, for ingesting prey, the difference in F-measure between a segment length of 0.4 vs 0.6 s is 0.728 vs 0.721, which is very small but despite that, it is significant (p=0.0003).  

# Plot mean confusion matrices using 0.4 s or flexible segments, based on 100 simulations  
RF.flex.output[[2]] # rows are predicted behaviours, columns observed behaviours
sensitivity.proportions.fixed <- apply(confusion.matrix.fixed.0.4.nsim, c(1,3), calculate.proportions)
precision.proportions.fixed <- apply(confusion.matrix.fixed.0.4.nsim, 1:2, calculate.proportions)
# return mean proportions:
sensitivity.search<-NA
for(i in 1:100) sensitivity.search[i] <- sensitivity.proportions.fixed[,i,]["for-search","for-search"]
mean(sensitivity.search) # 0.937
precision.search<-NA
for(i in 1:100) precision.search[i] <- precision.proportions.fixed[,i,]["for-search","for-search"]
mean(precision.search) # 0.879
sensitivity.proportion.mean.fixed <- apply(sensitivity.proportions.fixed, c(1,3), mean) 
precision.proportion.mean.fixed <- apply(precision.proportions.fixed, c(1,3), mean) 
# do the same for the flexible segmentation method
sensitivity.proportions.flex <- apply(confusion.matrix.flex.100.nsim, c(1,3), calculate.proportions)
precision.proportions.flex <- apply(confusion.matrix.flex.100.nsim, 1:2, calculate.proportions)
sensitivity.proportion.mean.flex <- apply(sensitivity.proportions.flex, c(1,3), mean) 
precision.proportion.mean.flex <- apply(precision.proportions.flex, c(1,3), mean) 
for (i in 1:10) {
  print(i)
  print(precision.proportions.flex[,i,])
} # sim 29 and 39 have all NA's. Not sure how this happened. Memory issue?
# this causes the means to also become all NA's. The simulations that have all NA's should thus be excluded from calculating the means. 
precision.proportion.mean.flex <- apply(precision.proportions.flex[,apply(precision.proportions.flex,2,all.NA)==F,], c(1,3), mean) 

#################
### FIGURE S6 ###
#################
pdf("output/FigS6.pdf",width=9,height=6)
layout(matrix(1:4,ncol=2))
par(mar=c(1,1,1,0), oma=c(6,4,3,14), las=1)
# plot sensitivity proportions for fixed segmentation
bardata <- barplot(sensitivity.proportion.mean.fixed[behaviour.pooled, behaviour.pooled], col=behaviour.colors, xlab="", ylab="", xaxt="n", cex.lab=1.3)
text(bardata, 1.05, round(mean.CRI.sensitivity.fixed.seglength[behaviour.labels.ordered, "0.4", "mean"],2), xpd=T)
mtext("Fixed segmentation", 3, 2, cex=1.3)
bardata <- barplot(precision.proportion.mean.fixed[behaviour.pooled, behaviour.pooled], col=behaviour.colors, xlab="", ylab="", xaxt="n")
axis(1,at=bardata,behaviour.labels.ordered.short, las=2)
text(bardata, 1.05, round(mean.CRI.precision.fixed.seglength[behaviour.labels.ordered, "0.4", "mean"],2), xpd=T)
bardata <- barplot(sensitivity.proportion.mean.flex[behaviour.pooled, behaviour.pooled], col=behaviour.colors, xlab="", ylab="", xaxt="n", yaxt="n")
text(bardata, 1.05, round(mean.CRI.sensitivity.flexible.ARL[behaviour.labels.ordered, "100", "mean"],2), xpd=T)
mtext("Flexible segmentation", 3, 2, cex=1.3)
mtext("Observed vs predicted", 4, 1, cex=1.3, las=0)
mtext("(sensitivity)", 4, 2.5, cex=1.3, las=0)
bardata <- barplot(precision.proportion.mean.flex[behaviour.pooled, behaviour.pooled], col=behaviour.colors, xlab="", ylab="", xaxt="n", yaxt="n")
text(bardata, 1.05, round(mean.CRI.precision.flexible.ARL[behaviour.labels.ordered, "100", "mean"],2), xpd=T)
mtext("Predicted vs observed", 4, 1, cex=1.3, las=0)
mtext("(precision)", 4, 2.5, cex=1.3, las=0)
axis(1,at=bardata,behaviour.labels.ordered.short, las=2)
legend(max(bardata)+4, 1, behaviour.labels.ordered.short, col=behaviour.colors, pch=15, bty="n", cex=1.3, xpd=NA)
dev.off()
### END FIGURE S6 ###

#################
### FIGURE S8 ###
#################
# Patterns of sensitivity and precision when using the number of segments available for each behaviour when using the maximum segment length of 2.0 s. 
pdf("output/FigS8.pdf",width=5,height=6)
layout(matrix(1:3, ncol=1))
par(mar=c(1,1,0,0), oma=c(5,5,1,10), xpd=T)
plot.statistic.nsim.behaviour.symbol(mean.CRI.sensitivity.fixed.seglength.small.sample, xlabel="", statistic="Sensitivity", plot.x=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.precision.fixed.seglength.small.sample, xlabel="", statistic="Precision", plot.x=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.Fmeasure.fixed.seglength.small.sample, xlabel="Fixed segment length (s)", statistic="F-measure", plot.x=T, x.numeric=F)
legend("bottomright", inset=c(-.4,0), legend=behaviour.labels.ordered.short, pch=point.type, xpd=NA, col=behaviour.colors)
dev.off()
### END FIGURE S8 ###

###############
### TABLE 1 ###
###############
### deviation in estimated prey intake rates (from observed prey intake rates) using fixed and flexible segmentation method applied to the annotated data
dev.ingestrate.fixed.mean <- round(colMeans(deviation.intakerate.fixed.seglength.nsim, na.rm=T), 2)
dev.ingestrate.fixed.lower <- round(apply(deviation.intakerate.fixed.seglength.nsim,2,quantile.0.025), 2)
dev.ingestrate.fixed.upper <- round(apply(deviation.intakerate.fixed.seglength.nsim,2,quantile.0.975), 2)
dev.ingestrate.flex.mean <- round(colMeans(deviation.intakerate.flexible.ARL.nsim, na.rm=T), 2)
dev.ingestrate.flex.lower <- round(apply(deviation.intakerate.flexible.ARL.nsim,2,quantile.0.025), 2)
dev.ingestrate.flex.upper <- round(apply(deviation.intakerate.flexible.ARL.nsim,2,quantile.0.975), 2)
# flexible segmentation method tends to slightly overestimate prey ingestion rates, whereas fixed segmentation mostly (except for 0.8 and 1.0 s segments) slightly underestimates.
table1 <- matrix("", nrow=12, ncol=3)
colnames(table1) <- c("FIXED","Segment length","Deviation of predicted from observed prey ingestion rate")
table1[8,] <- c("FLEXIBLE","ARL0 value","")
table1[1:length(seg.lengths),2] <- seg.lengths
table1[1:length(seg.lengths),3] <- paste(dev.ingestrate.fixed.mean, " (", dev.ingestrate.fixed.lower, "-", dev.ingestrate.fixed.upper, ")", sep="")
table1[8+1:length(ARL.values),2] <- ARL.values
table1[8+1:length(ARL.values),3] <- paste(dev.ingestrate.flex.mean, " (", dev.ingestrate.flex.lower, "-", dev.ingestrate.flex.upper, ")", sep="")
write.csv(table1, "output/Table1.csv", row.names=F) # col.names cannot be removed...
### END TABLE 1 ###

# Make plots per behaviour, to allow for a more precise comparison of patterns and overlap in 95% CRI
sensitivity.fixed.seglength.nsim[,"fly-soar","2"]<-NA
windows()
layout(matrix(1:8,ncol=2))
par(mar=c(2,3,2,1), oma=c(0,0,2,0))
for (i in behaviour.labels.ordered[1:8]) plot.statistic.per.behaviour(sensitivity.fixed.seglength.nsim, i)
mtext("Sensitivity - fixed",3,0,cex=1.5,outer=T)
windows()
layout(matrix(1:8,ncol=2))
par(mar=c(2,3,2,1), oma=c(0,0,2,0))
for (i in behaviour.labels.ordered[1:8]) plot.statistic.per.behaviour(precision.fixed.seglength.nsim, i)
mtext("Precision - fixed",3,0,cex=1.5,outer=T)
windows()
layout(matrix(1:8,ncol=2))
par(mar=c(2,3,2,1), oma=c(0,0,2,0))
for (i in behaviour.labels.ordered[1:8]) plot.statistic.per.behaviour(calculate.F.measure(sensitivity.fixed.seglength.nsim, precision.fixed.seglength.nsim), i)
mtext("F-measure - fixed",3,0,cex=1.5,outer=T)

# plot distribution of segment lengths for the different ARL0 values
pdf("output/FigS3.pdf",width=6,height=6)
layout(matrix(1:4, ncol=2, byrow=T))
par(mar=c(1,2,3,1),oma=c(4,4,0,0))
for (i in 1:length(df1.ARL.values)) {
  ARL0 <- ARL.values[i]
  seg.df <- df1.ARL.values[[i]]
  # distribution of length of all segments
  hist(seg.df$nobs.segments / 20, xlab="", xlim=c(0,1.6), ylim=c(0,2500), main="", las=1)
  mtext(substitute(paste(ARL[0]," = ", value), list(value=ARL0)),3,-1)
}
mtext('segment length (s)', 1, 2, outer=T)
mtext('frequency', 2, 2, outer=T)
dev.off()

# plot distribution of segment lengths per behaviour (9 behaviours)
unique(df1.ARL.values[[1]]$behaviour.pooled)
order.alt <- c(1:2,9,5:7,8,3:4)
pdf("output/FigS4.pdf",width=6,height=6)
layout(matrix(1:9, ncol=3, byrow=T))
par(mar=c(2,2,3,1),oma=c(4,4,0,0))
for (i in order.alt) {
  behaviour = behaviour.pooled[i]
  seg.df.behav <- df1.ARL.values[[1]][df1.ARL.values[[1]]$behaviour.pooled==behaviour,]
  # distribution of length of all segments
  hist(seg.df.behav$nobs.segments / 20, xlab="", xlim=c(0,1.6), main="", breaks=seq(0,1.6,0.1), las=1)
  mtext(behaviour.labels.ordered.short[i],3,0)
  # add the mean segment length
  text(15/20, 1000, paste('mean segment length = ', round(mean(seg.df$nobs.segments/20),2)))
}
mtext('segment length (s)', 1, 2, outer=T)
mtext('frequency', 2, 2, outer=T)
dev.off()