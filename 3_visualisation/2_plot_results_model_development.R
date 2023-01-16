## define point types used for different behaviours, in the order:
behaviour.pooled.ordered <- c("sit", "stand", "fly-active", "fly-passive", "search", "handle", "ingest", "walk", "drink")
behaviour.labels <- c("sit", "stand", "fly (active)", "fly (passive)", "search", "handle", "ingest", "walk", "drink")
point.type <- c(24,25,23,22,24,23,25,21,22) 
behaviour.colors <- brewer.pal(length(behaviour.labels), "Paired")
# switch colors for search (orange), handle (pink) and ingest (red):
behaviour.colors <- behaviour.colors[c(1:4,7,5,6,8:9)]

#################
### FIGURE S3 ###
#################
### distribution of segment lengths for the different ARL0 values ###
pdf("output/FigS3.pdf",width=6,height=6)
#windows()
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
### END FIGURE S3 ###

#################
### FIGURE S4 ###
#################
### distribution of segment lengths per behaviour (9 behaviours) ###
order.alt <- c(1:2,9,5:7,8,3:4)
pdf("output/FigS4.pdf",width=6,height=6)
#windows()
layout(matrix(1:9, ncol=3, byrow=T))
par(mar=c(2,2,3,1),oma=c(4,4,0,0))
for (i in order.alt) {
  behaviour = behaviour.pooled.ordered[i]
  seg.df.behav <- df1.ARL.values[[1]][df1.ARL.values[[1]]$behaviour.pooled==behaviour,]
  # distribution of length of all segments
  hist(seg.df.behav$nobs.segments / 20, xlab="", xlim=c(0,1.6), main="", breaks=seq(0,1.6,0.1), las=1)
  mtext(behaviour.labels[i],3,0)
  # add the mean segment length
  text(15/20, 1000, paste('mean segment length = ', round(mean(seg.df$nobs.segments/20),2)))
}
mtext('segment length (s)', 1, 2, outer=T)
mtext('frequency', 2, 2, outer=T)
dev.off()
### END FIGURE S4 ###

# Effect of maximum segment length (only a single round/simulation of model training and validation)
F.flexible.ARL.maxlength <- calculate.F.measure(sensitivity.flexible.ARL.maxlength, precision.flexible.ARL.maxlength)
windows(10,10)
layout(matrix(1:6, ncol=2, byrow=F))
par(mar=c(1,1,0,0), oma=c(5,5,3,8), xpd=T)
plot.statistic.single(sensitivity.flexible.ARL.maxlength[1,behaviour.pooled.ordered,], xlab="", statistic="Sensitivity", plot.x=F)
mtext("ARL=500",3,1,xpd=T)
plot.statistic.single(precision.flexible.ARL.maxlength[1,behaviour.pooled.ordered,], xlab="", statistic="Precision", plot.x=F)
plot.statistic.single(F.flexible.ARL.maxlength[1,behaviour.pooled.ordered,], xlab="Maximum segment length", statistic="F-measure")
plot.statistic.single(sensitivity.flexible.ARL.maxlength[2,behaviour.pooled.ordered,], xlab="", statistic="Sensitivity", plot.x=F, plot.y=F)
mtext("ARL=5000",3,1,xpd=T)
plot.statistic.single(precision.flexible.ARL.maxlength[2,behaviour.pooled.ordered,], xlab="", statistic="Precision", plot.x=F, plot.y=F)
plot.statistic.single(F.flexible.ARL.maxlength[2,behaviour.pooled.ordered,], xlab="Maximum segment length", statistic="F-measure", plot.y=F)
legend("bottomright", inset=c(-0.45,0), legend=behaviour.pooled.ordered, pch=point.type, xpd=NA, col=behaviour.colors)

# Effect startup value
F.effect.startup.value <- calculate.F.measure(sensitivity.effect.startup.value, precision.effect.startup.value)
windows(5,8)
layout(1:3)
par(mar=c(1,1,0,0), oma=c(4,4,3,8), xpd=T)
plot.statistic.single(t(sensitivity.effect.startup.value), xlabel="", statistic="Sensitivity", plot.x=F)
mtext("ARL=5000 and max seg length=1.6 s",3,1,xpd=T) # default ARL value used
plot.statistic.single(t(precision.effect.startup.value), xlabel="", statistic="Precision", plot.x=F)
plot.statistic.single(t(F.effect.startup.value), xlab="Startup value", statistic="F-measure")
legend("bottomright", inset=c(-0.45,0), legend=behaviour.labels, pch=point.type, xpd=NA, col=behaviour.colors)

#################
### FIGURE S5 ###
#################
### Effects of reduced predictor variable set and down- and upsampling of amount of different annotated behaviours
pdf("output/FigS5.pdf",width=9,height=6)
# windows(9,6)
layout(1:3)
par(mar=c(1,5,0,1), oma=c(2,0,13,0))
# Sensitivity
plot(c(1,length(behaviour.labels)), c(0,1), xlim=c(1,9.5), ylim=c(0,1), xaxt="n", pch=21, xlab="", ylab="Sensitivity", cex=2, las=1, bg="grey", type="n", cex.axis=1.3, cex.lab=1.5)
plot.statistic.nsim.behaviour.x(sensitivity.full.reduced.nsim.all[,behaviour.pooled.ordered,"full"], colour="blue", xadj=-0.2)
plot.statistic.nsim.behaviour.x(sensitivity.full.reduced.nsim.all[,behaviour.pooled.ordered,"reduced"], colour="green", xadj=-0.1)
plot.statistic.nsim.behaviour.x(sensitivity.stand4.nsim[,behaviour.pooled.ordered], colour="red", xadj=0)
plot.statistic.nsim.behaviour.x(sensitivity.walk10.nsim[,behaviour.pooled.ordered], colour="lightblue", xadj=0.1)
plot.statistic.nsim.behaviour.x(sensitivity.walk10.ingest6.nsim[,behaviour.pooled.ordered], colour="lavender", xadj=0.2)
plot.statistic.nsim.behaviour.x(sensitivity.stand4.search2.nsim[,behaviour.pooled.ordered], colour="orange", xadj=0.3)
plot.statistic.nsim.behaviour.x(sensitivity.stand4.search2.drink5.handle5.walk2.soar10.nsim[,behaviour.pooled.ordered], colour="purple", xadj=0.4)
plot.statistic.nsim.behaviour.x(sensitivity.search2.walk10.ingest6.nsim[,behaviour.pooled.ordered], colour="lightcoral", xadj=0.5)
plot.statistic.nsim.behaviour.x(sensitivity.stand4.search5.nsim[,behaviour.pooled.ordered], colour="pink", xadj=0.6)
legend("top", inset=c(-1,-1.35), legend=c("all predictors","only best predictors","DOWN stand 4x", "UP walk 10x", "UP walk 10x ingest 6x", "DOWN stand 4x, search 2x", "DOWN stand 4x, search 2x, UP drink 5x, handle 5x, walk 2x, fly-passive 10x", "DOWN search 2x, UP walk 10x, ingest 6x", "DOWN stand 4x, search 5x"), pch=21, xpd=NA, pt.bg=c("blue","green","red","lightblue","lavender","orange","purple","lightcoral","pink"), cex=1.2, pt.cex=2)
# Precision
plot(c(1,length(behaviour.labels)), c(0,1), xlim=c(1,9.5), ylim=c(0,1), xaxt="n", pch=21, xlab="", ylab="Precision", cex=2, las=1, bg="grey", type="n", cex.axis=1.3, cex.lab=1.5)
plot.statistic.nsim.behaviour.x(precision.full.reduced.nsim.all[,behaviour.pooled.ordered,"full"], colour="blue", xadj=-0.2)
plot.statistic.nsim.behaviour.x(precision.full.reduced.nsim.all[,behaviour.pooled.ordered,"reduced"], colour="green", xadj=-0.1)
plot.statistic.nsim.behaviour.x(precision.stand4.nsim[,behaviour.pooled.ordered], colour="red", xadj=0)
plot.statistic.nsim.behaviour.x(precision.walk10.nsim[,behaviour.pooled.ordered], colour="lightblue", xadj=0.1)
plot.statistic.nsim.behaviour.x(precision.walk10.ingest6.nsim[,behaviour.pooled.ordered], colour="lavender", xadj=0.2)
plot.statistic.nsim.behaviour.x(precision.stand4.search2.nsim[,behaviour.pooled.ordered], colour="orange", xadj=0.3)
plot.statistic.nsim.behaviour.x(precision.stand4.search2.drink5.handle5.walk2.soar10.nsim[,behaviour.pooled.ordered], colour="purple", xadj=0.4)
plot.statistic.nsim.behaviour.x(precision.search2.walk10.ingest6.nsim[,behaviour.pooled.ordered], colour="lightcoral", xadj=0.5)
plot.statistic.nsim.behaviour.x(precision.stand4.search5.nsim[,behaviour.pooled.ordered], colour="pink", xadj=0.6)
# F-measure
plot(c(1,length(behaviour.labels)), c(0,1), xlim=c(1,9.5), ylim=c(0,1), xaxt="n", pch=21, xlab="", ylab="F-measure", cex=2, las=1, bg="grey", type="n", cex.axis=1.3, cex.lab=1.5)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.full.reduced.nsim.all[,behaviour.pooled.ordered,"full"], precision.full.reduced.nsim.all[,behaviour.pooled.ordered,"full"]), colour="blue", xadj=-0.2)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.full.reduced.nsim.all[,behaviour.pooled.ordered,"reduced"], precision.full.reduced.nsim.all[,behaviour.pooled.ordered,"reduced"]), colour="green", xadj=-0.1)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.stand4.nsim[,behaviour.pooled.ordered], precision.stand4.nsim[,behaviour.pooled.ordered]), colour="red", xadj=0)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.walk10.nsim[,behaviour.pooled.ordered], precision.walk10.nsim[,behaviour.pooled.ordered]), colour="lightblue", xadj=0.1)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.walk10.ingest6.nsim[,behaviour.pooled.ordered], precision.walk10.ingest6.nsim[,behaviour.pooled.ordered]), colour="lavender", xadj=0.2)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.stand4.search2.nsim[,behaviour.pooled.ordered], precision.stand4.search2.nsim[,behaviour.pooled.ordered]), colour="orange", xadj=0.3)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.stand4.search2.drink5.handle5.walk2.soar10.nsim[,behaviour.pooled.ordered], precision.stand4.search2.drink5.handle5.walk2.soar10.nsim[,behaviour.pooled.ordered]), colour="purple", xadj=0.4)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.search2.walk10.ingest6.nsim[,behaviour.pooled.ordered], precision.search2.walk10.ingest6.nsim[,behaviour.pooled.ordered]), colour="lightcoral", xadj=0.5)
plot.statistic.nsim.behaviour.x(calculate.F.measure(sensitivity.stand4.search5.nsim[,behaviour.pooled.ordered], precision.stand4.search5.nsim[,behaviour.pooled.ordered]), colour="pink", xadj=0.6)
axis(1, at=1:9+0.2, labels=behaviour.labels, las=1, cex.axis=1.4)
dev.off()
### END FIGURE S5 ###

### Preparation for Figure S6: plot of mean confusion matrices using 0.4 s or flexible segments, based on 100 simulations ### 
# rows are predicted behaviours, columns observed behaviours
sensitivity.proportions.fixed <- apply(confusion.matrix.fixed.0.4.nsim, c(1,3), calculate.proportions)
precision.proportions.fixed <- apply(confusion.matrix.fixed.0.4.nsim, 1:2, calculate.proportions)
# return mean proportions:
sensitivity.search<-NA
for(i in 1:100) sensitivity.search[i] <- sensitivity.proportions.fixed[,i,]["search","search"]
mean(sensitivity.search) # 0.937
precision.search<-NA
for(i in 1:100) precision.search[i] <- precision.proportions.fixed[,i,]["search","search"]
mean(precision.search) # 0.879
sensitivity.proportion.mean.fixed <- apply(sensitivity.proportions.fixed, c(1,3), mean) 
precision.proportion.mean.fixed <- apply(precision.proportions.fixed, c(1,3), mean) 
# do the same for the flexible segmentation method
sensitivity.proportions.flex <- apply(confusion.matrix.flex.100.nsim, c(1,3), calculate.proportions)
precision.proportions.flex <- apply(confusion.matrix.flex.100.nsim, 1:2, calculate.proportions)
sensitivity.proportion.mean.flex <- apply(sensitivity.proportions.flex, c(1,3), mean) 
precision.proportion.mean.flex <- apply(precision.proportions.flex[,apply(precision.proportions.flex,2,all.NA)==F,], c(1,3), mean) 

#################
### FIGURE S6 ###
#################
### Visualisation of confusion matrices averaged over 100 simulations ###
pdf("output/FigS6.pdf",width=9,height=6)
#windows(9,6)
layout(matrix(1:4,ncol=2))
par(mar=c(1,1,1,0), oma=c(6,4,3,14), las=1)
# plot sensitivity proportions for fixed segmentation
bardata <- barplot(sensitivity.proportion.mean.fixed[behaviour.pooled.ordered, behaviour.pooled.ordered], col=behaviour.colors, xlab="", ylab="", xaxt="n", cex.lab=1.3)
text(bardata, 1.05, round(mean.CRI.sensitivity.fixed.seglength[behaviour.pooled.ordered, "0.4", "mean"],2), xpd=T)
mtext("Fixed segmentation", 3, 2, cex=1.3)
bardata <- barplot(precision.proportion.mean.fixed[behaviour.pooled.ordered, behaviour.pooled.ordered], col=behaviour.colors, xlab="", ylab="", xaxt="n")
axis(1,at=bardata,behaviour.labels, las=2)
text(bardata, 1.05, round(mean.CRI.precision.fixed.seglength[behaviour.pooled.ordered, "0.4", "mean"],2), xpd=T)
bardata <- barplot(sensitivity.proportion.mean.flex[behaviour.pooled.ordered, behaviour.pooled.ordered], col=behaviour.colors, xlab="", ylab="", xaxt="n", yaxt="n")
text(bardata, 1.05, round(mean.CRI.sensitivity.flexible.ARL[behaviour.pooled.ordered, "100", "mean"],2), xpd=T)
mtext("Flexible segmentation", 3, 2, cex=1.3)
mtext("Observed vs predicted", 4, 1, cex=1.3, las=0)
mtext("(sensitivity)", 4, 2.5, cex=1.3, las=0)
bardata <- barplot(precision.proportion.mean.flex[behaviour.pooled.ordered, behaviour.pooled.ordered], col=behaviour.colors, xlab="", ylab="", xaxt="n", yaxt="n")
text(bardata, 1.05, round(mean.CRI.precision.flexible.ARL[behaviour.pooled.ordered, "100", "mean"],2), xpd=T)
mtext("Predicted vs observed", 4, 1, cex=1.3, las=0)
mtext("(precision)", 4, 2.5, cex=1.3, las=0)
axis(1, at=bardata, behaviour.labels, las=2)
legend(max(bardata)+4, 1, behaviour.labels, col=behaviour.colors, pch=15, bty="n", cex=1.3, xpd=NA)
dev.off()
### END FIGURE S6 ###


#######################################################################################
### Comparison of model performance using fixed versus flexible segmentation method ###
#######################################################################################

### calculate mean, lcl and ucl over 100 simulations
mean.CRI.sensitivity.fixed.seglength <- calculate.mean.CRI(sensitivity.fixed.seglength.nsim)
mean.CRI.precision.fixed.seglength <- calculate.mean.CRI(precision.fixed.seglength.nsim)
F.measure.fixed.seglength.nsim <- calculate.F.measure(sensitivity.fixed.seglength.nsim, precision.fixed.seglength.nsim)
mean.CRI.Fmeasure.fixed.seglength <- calculate.mean.CRI(F.measure.fixed.seglength.nsim)
mean.CRI.sensitivity.flexible.ARL <- calculate.mean.CRI(sensitivity.flexible.ARL.nsim)
mean.CRI.precision.flexible.ARL <- calculate.mean.CRI(precision.flexible.ARL.nsim)
mean.CRI.Fmeasure.flexible.ARL <- calculate.mean.CRI(calculate.F.measure(sensitivity.flexible.ARL.nsim, precision.flexible.ARL.nsim))
### calculate mean, lcl and ucl over 10 simulations using for each segment length the same sample size as for the longest segment length (2.0 s) (to remove the potential confounding effect of smaller sample sizes at long fixed segments for behaviours that last relatively short, such as ingesting prey): 
mean.CRI.sensitivity.fixed.seglength.small.sample <- calculate.mean.CRI(sensitivity.fixed.seglength.nsim.small.sample)
mean.CRI.precision.fixed.seglength.small.sample <- calculate.mean.CRI(precision.fixed.seglength.nsim.small.sample)
mean.CRI.Fmeasure.fixed.seglength.small.sample <- calculate.mean.CRI(calculate.F.measure(sensitivity.fixed.seglength.nsim.small.sample, precision.fixed.seglength.nsim.small.sample))

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
legend("bottomright", inset=c(-.9,0), legend=behaviour.labels, pch=point.type, xpd=NA, col=behaviour.colors)
dev.off()
### END FIGURE 3 ###

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
table1 <- matrix("", nrow=12, ncol=3)
colnames(table1) <- c("FIXED","Segment length","Deviation of predicted from observed prey ingestion rate")
table1[8,] <- c("FLEXIBLE","ARL0 value","")
table1[1:length(seg.lengths),2] <- seg.lengths
table1[1:length(seg.lengths),3] <- paste(dev.ingestrate.fixed.mean, " (", dev.ingestrate.fixed.lower, "-", dev.ingestrate.fixed.upper, ")", sep="")
table1[8+1:length(ARL.values),2] <- ARL.values
table1[8+1:length(ARL.values),3] <- paste(dev.ingestrate.flex.mean, " (", dev.ingestrate.flex.lower, "-", dev.ingestrate.flex.upper, ")", sep="")
write.csv(table1, "output/Table1.csv", row.names=F)
### END TABLE 1 ###


#################
### FIGURE S8 ###
#################
### Patterns of sensitivity, precision and F-measure when using the number of segments available for each behaviour when using the maximum segment length of 2.0 s ### 
pdf("output/FigS8.pdf",width=5,height=6)
#windows(5,6)
layout(matrix(1:3, ncol=1))
par(mar=c(1,1,0,0), oma=c(5,5,1,10), xpd=T)
plot.statistic.nsim.behaviour.symbol(mean.CRI.sensitivity.fixed.seglength.small.sample, xlabel="", statistic="Sensitivity", plot.x=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.precision.fixed.seglength.small.sample, xlabel="", statistic="Precision", plot.x=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.Fmeasure.fixed.seglength.small.sample, xlabel="Fixed segment length (s)", statistic="F-measure", plot.x=T, x.numeric=F)
legend("bottomright", inset=c(-.4,0), legend=behaviour.labels, pch=point.type, xpd=NA, col=behaviour.colors)
dev.off()
### END FIGURE S8 ###

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

##################
### FIGURE S11 ###
##################
### Effect of sampling frequency on classification performance
pdf("output/FigS11.pdf", width=4, height=6)
#windows(4,6) 
layout(matrix(1:3, ncol=1, byrow=F))
par(mar=c(1,1,0,10), oma=c(5,5,1,0), xpd=T)
plot.statistic.nsim.behaviour.symbol(mean.CRI.sensitivity.sampling.freq, xlabel="", statistic="Sensitivity", plot.x=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.precision.sampling.freq, xlabel="", statistic="Precision", plot.x=F, x.numeric=F)
plot.statistic.nsim.behaviour.symbol(mean.CRI.F.measure.sampling.freq, xlabel="Sampling frequency (Hz)", statistic="F-measure", x.numeric=F)
legend("bottomright", inset=c(-0.6,0), legend=behaviour.labels, pch=point.type, xpd=NA, col=behaviour.colors)
dev.off()
### End Figure S11 ###

### Classification performance of broad-scale behavours (rest, fly-passive, fly-active, and active-on-the-ground) ###
behaviours.broad <- dimnames(mean.CRI.sensitivity.pooled.behaviours.sampling.freq)[[1]]
windows(4,6) 
layout(matrix(1:3, ncol=1, byrow=F))
par(mar=c(1,1,0,10), oma=c(5,5,1,0), xpd=T)
plot.statistic.nsim.behaviour.symbol(mean.CRI.sensitivity.pooled.behaviours.sampling.freq, xlabel="", statistic="Sensitivity", plot.x=F, x.numeric=F, behaviours=behaviours.broad, colors=behaviour.colors[c(3,4,8,2)])
plot.statistic.nsim.behaviour.symbol(mean.CRI.precision.pooled.behaviours.sampling.freq, xlabel="", statistic="Precision", plot.x=F, x.numeric=F, behaviours=behaviours.broad, colors=behaviour.colors[c(3,4,8,2)])
plot.statistic.nsim.behaviour.symbol(mean.CRI.F.measure.pooled.behaviours.sampling.freq, xlabel="Sampling frequency (Hz)", statistic="F-measure", x.numeric=F, behaviours=behaviours.broad, colors=behaviour.colors[c(3,4,8,2)])
legend("bottomright", inset=c(-0.6,0), legend=behaviours.broad, pch=point.type, xpd=NA, col=behaviour.colors[c(3,4,8,2)])
