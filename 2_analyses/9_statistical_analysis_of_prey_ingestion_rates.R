# only select samples with 32 points (1.6 s), for a fair comparison between the two segmentation methods, as with flexible segmentation, samples < 32 points are kicked out.acc.sample.length.bird.datetime <- aggregate(nobs.segments~date_time+BirdID, df.53.hab.fixed, sum)  
acc.sample.length.bird.datetime <- aggregate(nobs.segments~date_time+BirdID, df.53.hab.fixed, sum)  
names(acc.sample.length.bird.datetime)[3] <- "acc.sample.length"
df.53.hab.fixed <- merge(df.53.hab.fixed, acc.sample.length.bird.datetime[,1:3], all.x=T)
df.53.hab.fixed <- df.53.hab.fixed[df.53.hab.fixed$acc.sample.length==32,] 

# to allow for a comparable analysis for the fixed and flexible segments, there are two options: 
# 1. DURATION APPROACH: I will calculate the time spent ingesting prey vs searching/handling prey at the resolution of interest (yday, daynight, yday.timetolow). This would be relatively easy when looking at day/night and yday, but will get complex if we additionally want to add time until low tide. 
# 2. BINOMIAL APPROACH: I will assume that if there was a prey ingestion in any of the segments of the 1.6 s acc-samples, that we assume that a prey was ingested during this date_time event. This may somewhat overestimate prey ingestion rate, as prey ingestions usually last shorter than 1.6 s (i.e. 0.8 s on average)... However, for comparison between the fixed and flexible segmentation method, this may be the easiest way.  

### 1. DURATION APPROACH ###
# investigate the proportion of time spent foraging, over the season and in the waddensea
# calculate proportion of time spent foraging per 24 hours per individual

# proportion of foraging time spent in the Wadden Sea:
df.53.hab.fixed$wadden <- ifelse(is.na(df.53.hab.fixed$Name)==T,0,1)
df.53.hab.fixed$wadden2 <- ifelse(df.53.hab.fixed$habitat=="waddenzee",1,0)
df.53.hab.fixed$nobs.wadden <- df.53.hab.fixed$wadden*df.53.hab.fixed$nobs.segments
df.foraging <- df.53.hab.fixed[df.53.hab.fixed$foraging==1,]
prop.foraging.wadden <- aggregate(cbind(nobs.wadden,nobs.segments)~year+month+yday+BirdID+sex, df.foraging, sum)
prop.foraging.wadden$prop.for.wadden <- round(prop.foraging.wadden$nobs.wadden/prop.foraging.wadden$nobs.segments,3)
aggregate(prop.for.wadden~sex, prop.foraging.wadden, mean) # females spent 70% of their foraging time in the Wadden Sea and males 42%. 

# for females, the Wadden Sea is clearly their primary foraging habitats, we therefore restrict our analysis to females, also to exclude potentially confounding effects of body size and different foraging sites/diets, with males being larger
# select females only
df.53.hab.females <- df.53.hab.fixed[df.53.hab.fixed$sex=="F",]
df.53.hab.females$ydayz <- (df.53.hab.females$yday-mean(df.53.hab.females$yday))/sd(df.53.hab.females$yday)
df.53.hab.females$ydayz_sq <- df.53.hab.females$ydayz^2

df.foraging.wadden <- df.53.hab.females[is.na(df.53.hab.females$Name)==F&df.53.hab.females$foraging==1,] # Name is the name of the kombergingsgebied within the Wadden Sea

# foraging success as a function of season
for.suc.season <- aggregate(cbind(nobs.foraging,nobs.ingest)~yday+ydayz+ydayz_sq+BirdID+month+year, df.foraging.wadden, sum)
for.suc.season$for.suc <- for.suc.season$nobs.ingest/for.suc.season$nobs.foraging
# select only days with sufficient data: 
for.suc.season <- for.suc.season[for.suc.season$nobs.foraging>400,]
# transform foraging success on logit scale:
for.suc.min <- min(for.suc.season$for.suc[for.suc.season$for.suc>0])
for.suc.max <- max(for.suc.season$for.suc[for.suc.season$for.suc<1])
for.suc.season$for.suc.logit <- qlogis(for.suc.season$for.suc)
for.suc.season$for.suc.logit[for.suc.season$for.suc==0] <- qlogis(for.suc.min)
for.suc.season$for.suc.logit[for.suc.season$for.suc==1] <- qlogis(for.suc.max)
for.suc.season$yearf <- as.factor(for.suc.season$year)
mean.for.suc.season <- calculate.mean.95CI.from.logits(for.suc.season)

# run lme
for.suc.season <- for.suc.season[order(for.suc.season$yday),]
model.seasonxyr.rnd.slopes <- lme(for.suc.logit~(ydayz+ydayz_sq)*yearf, for.suc.season, random=~(ydayz+ydayz_sq)|BirdID, method="ML")
model.seasonxyr.rnd.int <- lme(for.suc.logit~(ydayz+ydayz_sq)*yearf, for.suc.season, random=~1|BirdID, method="ML")
model.seasonxyrixed <- lm(for.suc.logit~(ydayz+ydayz_sq)*yearf, for.suc.season)
AIC(model.seasonxyr.rnd.slopes, model.seasonxyr.rnd.int, model.seasonxyrixed) # strong support for individual variation in slopes. 
# running models with random slopes when season is included
model.season.rnd.slopes <- lme(for.suc.logit~ydayz+ydayz_sq, for.suc.season, random=~(ydayz+ydayz_sq)|BirdID, method="ML")
model.season.yr.rnd.slopes <- lme(for.suc.logit~ydayz+ydayz_sq+yearf, for.suc.season, random=~(ydayz+ydayz_sq)|BirdID, method="ML")
# running models without season with only random intercept: (see: https://stats.stackexchange.com/questions/532209/do-i-have-to-drop-a-random-slope-if-i-drop-the-fixed-effect-in-mixed-models-for#)
model.yr.rnd.int <- lme(for.suc.logit~yearf, for.suc.season, random=~1|BirdID, method="ML")
model.c.rnd.int <- lme(for.suc.logit~1, for.suc.season, random=~1|BirdID, method="ML")
for.suc.season$predict.ydaysqxyr.pop <- plogis(predict(model.seasonxyr.rnd.slopes, level=0))
for.suc.season$predict.ydaysq.pop <- plogis(predict(model.season.rnd.slopes, level=0))
Table2 <- AICc(model.seasonxyr.rnd.slopes, model.season.rnd.slopes, model.season.yr.rnd.slopes, model.yr.rnd.int, model.c.rnd.int)
Table2$dAICc <- Table2$AICc-min(Table2$AICc)
Table2$expmin0.5dAICc <- exp(-0.5*Table2$dAICc)
Table2$Akaikeweight <- round(Table2$expmin0.5dAICc/sum(Table2$expmin0.5dAICc),2)
Table2$dAICc <- round(Table2$dAICc,2) # only round after calculating Akaikeweight
Table2 <- Table2[order(Table2$dAICc),]
row.names(Table2) <- c("season * year","season + year","season","year","-")
write.csv(Table2[,c("df","dAICc","Akaikeweight")], "output/Table2.csv")
write.table(Table2[,c("df","dAICc","Akaikeweight")], "clipboard", sep="\t")

# data per month per bird per year:
TableS3 <- table(for.suc.season$BirdID, for.suc.season$month, for.suc.season$year) # make a coloured table out of this. 
TableS3 <- cbind(TableS3[,,1],TableS3[,,2],TableS3[,,3],TableS3[,,4])
write.csv(TableS3, "output/TableS3.csv")

################################ FIGURE 4 ######################################## 
### Seasonal variation in prey ingestion rates using fixed segmentation method ###
##################################################################################
pdf("output/Fig4.pdf",height=6,width=6)
#windows()
par(mar=c(1,4,0,1), oma=c(2,0,1,0))
layout(matrix(1:2,ncol=1))
# to translate proportion to prey ingestion rate, apply "*60/0.8"
# PANEL A
plotCI(mean.for.suc.season$yday, mean.for.suc.season$mean*60/0.8, li=mean.for.suc.season$li*60/0.8, ui=mean.for.suc.season$ui*60/0.8, xlab='', ylab = '', ylim=c(0,0.2*60/0.8), xlim=c(min(mean.for.suc.season$yday),max(mean.for.suc.season$yday)), pch=21, sfrac=0, gap=0, pt.bg="grey80", cex=0.8, xaxt='n', las=1)
lines(predict.ydaysq.pop*60/0.8~yday, data=for.suc.season, lwd=2)
text(min(mean.for.suc.season$yday), 14.5,"(a)",cex=1.3)
# PANEL B
plot(for.suc.season$yday, for.suc.season$for.suc*60/0.8, xlab='', ylab = '', xlim=c(min(for.suc.season$yday),max(for.suc.season$yday)), ylim=c(0,.2*60/0.8), xaxt='n', las=1, type="n")
text(min(mean.for.suc.season$yday), 14.5,"(b)",cex=1.3)
for (i in 2016:2019) {
  mean.for.suc.season.yeari <- calculate.mean.95CI.from.logits(for.suc.season[for.suc.season$year==i,])
  points(mean.for.suc.season.yeari$yday, mean.for.suc.season.yeari$mean*60/0.8, pch=21, col=c("red","blue","green","orange")[i-2015], cex=0.8)
}
for (i in rev(2016:2019)) lines(for.suc.season$yday[for.suc.season$year==i], for.suc.season$predict.ydaysqxyr.pop[for.suc.season$year==i]*60/0.8, lwd=3, col=c("red","blue","green","orange")[i-2015]) # to make the distinct seasonal pattern of 2016 best visible
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=c("1 Apr","1 May","1 June","1 July","1 Aug","1 Sep","1 Oct"))
mtext(expression("Prey ingestion rate ( "*min^-1*" )"),2,-1.5,cex=1.2,outer=T)
legend("topright", legend=2016:2019, text.col=c("red","blue","green","orange"), bty="n")
dev.off()
############################### END FIGURE 4 ####################################