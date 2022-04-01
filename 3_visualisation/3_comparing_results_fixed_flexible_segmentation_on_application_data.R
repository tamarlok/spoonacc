### compare time allocation and estimated prey ingestion rates for fixed and flexible segmentation ###

# Preparation for Figure S5
# FIXED
df.foraging.wadden.fixed <- df.53.hab.fixed[df.53.hab.fixed$wadden==1&df.53.hab.fixed$sex=="F"&df.53.hab.fixed$foraging==1,] 
for.suc.yday.fixed <- aggregate(cbind(nobs.foraging,nobs.ingest,foraging)~yday+BirdID+year, df.foraging.wadden.fixed, sum)
# only select estimates based on >20 s acceleration data classified as foraging (where nobs = # seconds x 20 Hz):
for.suc.yday.fixed <- for.suc.yday.fixed[for.suc.yday.fixed$nobs.foraging>20*20,]
for.suc.yday.fixed$ingest.rate <- for.suc.yday.fixed$nobs.ingest/for.suc.yday.fixed$nobs.foraging/0.8*60
mean.for.suc.yday.fixed <- calculate.mean.95CI.from.log(for.suc.yday.fixed)
# FLEXIBLE
df.foraging.wadden.flex <- df.53.hab.flex[df.53.hab.flex$wadden==1&df.53.hab.flex$foraging==1&df.53.hab.flex$sex=="F",]
for.suc.yday.flex <- aggregate(cbind(nobs.foraging,nobs.ingest)~yday+BirdID+year, df.foraging.wadden.flex, sum)
for.suc.yday.flex <- for.suc.yday.flex[for.suc.yday.flex$nobs.foraging>20*20,] 
for.suc.yday.flex$ingest.rate <- for.suc.yday.flex$nobs.ingest/for.suc.yday.flex$nobs.foraging/0.8*60
mean.for.suc.yday.flex <- calculate.mean.95CI.from.log(for.suc.yday.flex)

#################
### FIGURE S5 ###
#################
# plot means with 95% CI
pdf("output/FigS5.pdf",width=9,height=7)
#windows(9,7)
par(mar=c(2,2,1,0), oma=c(2,2,0,1))
# FIXED
plotCI(mean.for.suc.yday.fixed$yday, mean.for.suc.yday.fixed$mean, li=mean.for.suc.yday.fixed$li, ui=mean.for.suc.yday.fixed$ui, xlab="", ylab = "", ylim=c(0,15), xlim=c(min(mean.for.suc.yday.fixed$yday),max(mean.for.suc.yday.fixed$yday)), pch=21, sfrac=0, gap=0, pt.bg="white", col="steelblue2", xaxt='n', las=1)
# FLEXIBLE
plotCI(mean.for.suc.yday.flex$yday+0.5, mean.for.suc.yday.flex$mean, li=mean.for.suc.yday.flex$li, ui=mean.for.suc.yday.flex$ui, add=T, pch=21, sfrac=0, gap=0, col="orange2", pt.bg="white", xaxt='n', ylim=c(0,0.3))
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=c("1 Apr","1 May","1 June","1 July","1 Aug","1 Sep","1 Oct"))
mtext(expression("Prey ingestion rate ( "*min^-1*" )"),2,2.5,cex=1.2)
dev.off()
### END FIGURE S5 ###

median((mean.for.suc.yday.fixed$mean/mean.for.suc.yday.flex$mean)) # ratio 1.38
median((mean.for.suc.yday.flex$mean/mean.for.suc.yday.fixed$mean)) # ratio 0.73
median((mean.for.suc.yday.fixed$mean-mean.for.suc.yday.flex$mean)/mean(c(mean.for.suc.yday.fixed$mean,mean.for.suc.yday.flex$mean))) # 33% higher estimated prey ingestion rates using fixed compared to flexible segmentation method

# Data preparation for Figure S7
# overall time budget fixed segmentation:
prop.foraging.day.fixed <- aggregate(cbind(nobs.foraging,nobs.ingest,nobs.segments)~yday, df.53.hab.fixed[df.53.hab.fixed$month%in%4:9,], sum)
prop.foraging.day.fixed$prop.for <- round(prop.foraging.day.fixed$nobs.foraging/prop.foraging.day.fixed$nobs.segments,3)
prop.foraging.day.fixed$prop.ingest <- round(prop.foraging.day.fixed$nobs.ingest/prop.foraging.day.fixed$nobs.segments,3)
prop.foraging.day.fixed$prop.search <- round((prop.foraging.day.fixed$nobs.for-prop.foraging.day.fixed$nobs.ingest)/prop.foraging.day.fixed$nobs.segments,3)
prop.foraging.day.fixed$ingest.rate <- round(prop.foraging.day.fixed$nobs.ingest/prop.foraging.day.fixed$nobs.foraging,3)/0.8*60
# overall time budget flexible segmentation:
prop.foraging.day.flex <- aggregate(cbind(nobs.foraging,nobs.ingest,nobs.segments)~yday, df.53.hab.flex[df.53.hab.flex$month%in%4:9,], sum)
prop.foraging.day.flex$prop.for <- round(prop.foraging.day.flex$nobs.foraging/prop.foraging.day.flex$nobs.segments,3)
prop.foraging.day.flex$prop.ingest <- round(prop.foraging.day.flex$nobs.ingest/prop.foraging.day.flex$nobs.segments,3)
prop.foraging.day.flex$prop.search <- round((prop.foraging.day.flex$nobs.for-prop.foraging.day.flex$nobs.ingest)/prop.foraging.day.flex$nobs.segments,3)
prop.foraging.day.flex$ingest.rate <- round(prop.foraging.day.flex$nobs.ingest/prop.foraging.day.flex$nobs.foraging,3)/0.8*60
# time budget when in the Wadden Sea, fixed segmentation:
prop.foraging.day.wadden.fixed <- aggregate(cbind(nobs.foraging,nobs.ingest,nobs.segments)~yday, df.53.hab.fixed[df.53.hab.fixed$month%in%4:9&df.53.hab.fixed$wadden==1,], sum)
prop.foraging.day.wadden.fixed$prop.for <- round(prop.foraging.day.wadden.fixed$nobs.foraging/prop.foraging.day.wadden.fixed$nobs.segments,3)
prop.foraging.day.wadden.fixed$prop.ingest <- round(prop.foraging.day.wadden.fixed$nobs.ingest/prop.foraging.day.wadden.fixed$nobs.segments,3)
prop.foraging.day.wadden.fixed$prop.search <- round((prop.foraging.day.wadden.fixed$nobs.for-prop.foraging.day.wadden.fixed$nobs.ingest)/prop.foraging.day.wadden.fixed$nobs.segments,3)
prop.foraging.day.wadden.fixed$ingest.rate <- round(prop.foraging.day.wadden.fixed$nobs.ingest/prop.foraging.day.wadden.fixed$nobs.foraging,3)/0.8*60
# time budget when in the Wadden Sea, flexible segmentation:
prop.foraging.day.wadden.flex <- aggregate(cbind(nobs.foraging,nobs.ingest,nobs.segments)~yday, df.53.hab.flex[df.53.hab.flex$month%in%4:9&df.53.hab.flex$wadden==1,], sum)
prop.foraging.day.wadden.flex$prop.for <- round(prop.foraging.day.wadden.flex$nobs.foraging/prop.foraging.day.wadden.flex$nobs.segments,3)
prop.foraging.day.wadden.flex$prop.ingest <- round(prop.foraging.day.wadden.flex$nobs.ingest/prop.foraging.day.wadden.flex$nobs.segments,3)
prop.foraging.day.wadden.flex$prop.search <- round((prop.foraging.day.wadden.flex$nobs.for-prop.foraging.day.wadden.flex$nobs.ingest)/prop.foraging.day.wadden.flex$nobs.segments,3)
prop.foraging.day.wadden.flex$ingest.rate <- round(prop.foraging.day.wadden.flex$nobs.ingest/prop.foraging.day.wadden.flex$nobs.foraging,3)/0.8*60

#################
### Figure S7 ###
#################
# plot proportion of time spent foraging and ingesting and the subsequent prey ingestion rate estimated by the two segmentation methods:
pdf("output/FigS7.pdf", width=10, height=7)
#windows(10,7)
# all data
layout(matrix(1:6,ncol=2,byrow=F))
par(mar=c(1,5,0,0), oma=c(1,0,3,1), mgp=c(3.5,1,0))
plot(prop.search~yday, prop.foraging.day.flex, ylab="Perc. time searching", pch=21, col="orange2", xaxt="n", yaxt="n", ylim=c(0.20,0.50), cex.lab=1.5)
mtext("All data",3,1,cex=1.3)
points(prop.search~yday, prop.foraging.day.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(2, at=seq(0,0.5,0.1), labels=paste(seq(0,50,10),"%",sep=""), line=0, las=1, cex.axis=1.3)
legend("bottomleft", c("fixed","flexible"), pch=21, col=c("steelblue2","orange2"), bty="n", cex=1.5)
plot(prop.ingest~yday, prop.foraging.day.flex, ylab="Perc. time ingesting", pch=21, col="orange2", xaxt="n", yaxt="n", las=1, ylim=c(0,0.05), cex.lab=1.5)
points(prop.ingest~yday, prop.foraging.day.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(2, at=seq(0,0.05,0.01), labels=paste(seq(0,5,1),"%",sep=""), line=0, las=1, cex.axis=1.3)
plot(ingest.rate~yday, prop.foraging.day.flex, ylab="", pch=21, col="orange2", ylim=c(0,10), xaxt="n", las=1, cex.lab=1.5, cex.axis=1.3)
mtext(expression("Prey ingestion rate ( "*min^-1*" )"),2,3)
points(ingest.rate~yday, prop.foraging.day.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1)+15), labels=c("Apr","May","June","July","Aug","Sep"), tick=F, line=-0.5, cex.axis=1.3)
# Wadden Sea data:
plot(prop.search~yday, prop.foraging.day.wadden.flex, ylab="", pch=21, col="orange2", xaxt="n", yaxt="n", ylim=c(0.25,0.75), cex.lab=1.5)
mtext("Wadden Sea data",3,1,cex=1.3)
points(prop.search~yday, prop.foraging.day.wadden.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(2, at=seq(0,1,0.1), labels=paste(seq(0,100,10),"%",sep=""), line=0, las=1, cex.axis=1.3)
plot(prop.ingest~yday, prop.foraging.day.wadden.flex, ylab="", pch=21, col="orange2", xaxt="n", yaxt="n", las=1, ylim=c(0,0.11), cex.lab=1.5)
points(prop.ingest~yday, prop.foraging.day.wadden.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(2, at=seq(0,0.10,0.02), labels=paste(seq(0,10,2),"%",sep=""), line=0, las=1, cex.axis=1.3)
plot(ingest.rate~yday, prop.foraging.day.wadden.flex, ylab="", pch=21, col="orange2", ylim=c(0,12), xaxt="n", las=1, cex.lab=1.5, cex.axis=1.3)
points(ingest.rate~yday, prop.foraging.day.wadden.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1)+15), labels=c("Apr","May","June","July","Aug","Sep"), tick=F, line=-0.5, cex.axis=1.3)
dev.off()
### END Figure S7 ### 

# Data preparation for Figure S8
# calculate time budgets for fixed and flexible segmentation in table format:
dur.behav.yday.fixed.xtabs = xtabs(nobs.segments~pred.behav+yday, data=df.53.hab.fixed[df.53.hab.fixed$month%in%4:9,])
prop.behav.yday.fixed <- prop.table(dur.behav.yday.fixed.xtabs,2)
dur.behav.yday.flex.xtabs = xtabs(nobs.segments~pred.behav+yday, data=df.53.hab.flex[df.53.hab.flex$month%in%4:9,])
prop.behav.yday.flex <- prop.table(dur.behav.yday.flex.xtabs,2)

#################
### Figure S8 ###
#################
pdf("output/FigS8.pdf", width=10, height=4)
layout(matrix(1:2, ncol=2))
par(mar=c(1,1,2,0), oma=c(1,3,0,8))
bardata <- barplot(prop.behav.yday.fixed[behaviour.pooled,], main='', col=behaviour.colors, las=1, xaxt='n', yaxt='n', border=NA, ylab="", space=0)
axis(1, at=bardata[15+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1)], labels=c("Apr","May","June","July","Aug","Sep"), tick=F, line=-1)
axis(2, at=seq(0,1,0.2), labels=paste(seq(0,100,20),"%",sep=""), line=-0.5, las=1)
mtext('Percentage of time', 2, 3, cex=1.2)
mtext('FIXED', 3, 0.6, cex=1)
barplot(prop.behav.yday.flex[behaviour.pooled,], main='', col=behaviour.colors, las=1, xaxt='n', yaxt="n", border=NA, space=0)
mtext('FLEXIBLE', 3, 0.6, cex=1)
legend(190, 0.6, rev(behaviour.labels.ordered), pch=15, col=rev(behaviour.colors), bty='n', xpd=NA, cex=1)
axis(1, at=bardata[15+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1)], labels=c("Apr","May","June","July","Aug","Sep"), tick=F, line=-1)
dev.off()
### End Figure S8 ###