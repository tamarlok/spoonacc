# only select samples with 32 points (1.6 s), for a fair comparison between the two segmentation methods, as with flexible segmentation, samples < 32 points are kicked out
acc.sample.length.bird.datetime <- aggregate(nobs.segments~date_time+BirdID, df.53.hab.fixed, sum)  
names(acc.sample.length.bird.datetime)[3] <- "acc.sample.length"
df.53.hab.fixed <- merge(df.53.hab.fixed, acc.sample.length.bird.datetime[,1:3], all.x=T)
df.53.hab.fixed <- df.53.hab.fixed[df.53.hab.fixed$acc.sample.length==32,] 

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
ingest.rate.season <- aggregate(cbind(nobs.foraging,nobs.ingest)~yday+ydayz+ydayz_sq+BirdID+month+year, df.foraging.wadden, sum)
ingest.rate.season$prop.for.ingest <- ingest.rate.season$nobs.ingest/ingest.rate.season$nobs.foraging
ingest.rate.season$ingest.rate <- ingest.rate.season$prop.for.ingest/0.8*60
hist(ingest.rate.season$prop.for.ingest, breaks=20)
hist(ingest.rate.season$ingest.rate, breaks=15)
# select only days with sufficient data: 
ingest.rate.season <- ingest.rate.season[ingest.rate.season$nobs.foraging>400,]
# transform foraging success on logit scale:
for.suc.min <- min(ingest.rate.season$ingest.rate[ingest.rate.season$ingest.rate>0])
ingest.rate.season$log.ingest.rate <- log(ingest.rate.season$ingest.rate+for.suc.min)
hist(ingest.rate.season$log.ingest.rate, breaks=20)
ingest.rate.season$yearf <- as.factor(ingest.rate.season$year)
mean.ingest.rate.season <- calculate.mean.95CI.from.log(ingest.rate.season)

# run lme
ingest.rate.season <- ingest.rate.season[order(ingest.rate.season$yday),]
model.seasonxyr.rnd.slopes <- lme(log.ingest.rate~(ydayz+ydayz_sq)*yearf, ingest.rate.season, random=~(ydayz+ydayz_sq)|BirdID, method="ML")
model.seasonxyr.rnd.int <- lme(log.ingest.rate~(ydayz+ydayz_sq)*yearf, ingest.rate.season, random=~1|BirdID, method="ML")
model.seasonxyrixed <- lm(log.ingest.rate~(ydayz+ydayz_sq)*yearf, ingest.rate.season)
AIC(model.seasonxyr.rnd.slopes, model.seasonxyr.rnd.int, model.seasonxyrixed) # strong support for individual variation in slopes. 
# running models with random slopes when season is included
model.season.rnd.slopes <- lme(log.ingest.rate~ydayz+ydayz_sq, ingest.rate.season, random=~(ydayz+ydayz_sq)|BirdID, method="ML")
model.season.yr.rnd.slopes <- lme(log.ingest.rate~ydayz+ydayz_sq+yearf, ingest.rate.season, random=~(ydayz+ydayz_sq)|BirdID, method="ML")
# running models without season with only random intercept: (see: https://stats.stackexchange.com/questions/532209/do-i-have-to-drop-a-random-slope-if-i-drop-the-fixed-effect-in-mixed-models-for#)
model.yr.rnd.int <- lme(log.ingest.rate~yearf, ingest.rate.season, random=~1|BirdID, method="ML")
model.c.rnd.int <- lme(log.ingest.rate~1, ingest.rate.season, random=~1|BirdID, method="ML")
ingest.rate.season$predict.ydaysqxyr.pop <- exp(predict(model.seasonxyr.rnd.slopes, level=0))
ingest.rate.season$predict.ydaysq.pop <- exp(predict(model.season.rnd.slopes, level=0))

# run additive models to allow the seasonal effect to be estimated by a smoother:
model.seasongam <- gam(log.ingest.rate~s(ydayz), data=ingest.rate.season, method="ML")
windows()
plot(model.seasongam)
summary(model.seasongam)
model.seasongamm4.rnd.int <- gamm4(log.ingest.rate~s(ydayz), random=~(1|BirdID), data=ingest.rate.season, REML=F)
plot(model.seasongamm4.rnd.int$gam)
model.seasongamm.rnd.int <- gamm(log.ingest.rate~s(ydayz), random=list(BirdID=~1), data=ingest.rate.season, method="ML")
plot(model.seasongamm.rnd.int$gam)
anova(model.seasongamm4.rnd.int$gam)
, model.seasongamm.rnd.int$gam)

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
TableS3 <- table(ingest.rate.season$BirdID, ingest.rate.season$month, ingest.rate.season$year) # make a coloured table out of this. 
TableS3 <- cbind(TableS3[,,1],TableS3[,,2],TableS3[,,3],TableS3[,,4])
TOTAL <- colSums(TableS3)
TableS3 <- rbind(TableS3, TOTAL)
write.csv(TableS3, "output/TableS3.csv")

################################ FIGURE 5 ######################################## 
### Seasonal variation in prey ingestion rates using fixed segmentation method ###
##################################################################################
pdf("output/Fig5.pdf",height=6,width=6)
#windows()
par(mar=c(1,4,0,1), oma=c(2,0,1,0))
layout(matrix(1:2,ncol=1))
# PANEL A
plotCI(mean.ingest.rate.season$yday, mean.ingest.rate.season$mean, li=mean.ingest.rate.season$li, ui=mean.ingest.rate.season$ui, xlab='', ylab = '', ylim=c(0,15), xlim=c(min(mean.ingest.rate.season$yday),max(mean.ingest.rate.season$yday)), pch=21, sfrac=0, gap=0, pt.bg="grey80", cex=0.8, xaxt='n', las=1)
lines(predict.ydaysq.pop~yday, data=ingest.rate.season, lwd=2)
text(min(mean.ingest.rate.season$yday), 14.5,"(a)",cex=1.3)
# PANEL B
plot(ingest.rate.season$yday, ingest.rate.season$for.suc, xlab='', ylab = '', xlim=c(min(ingest.rate.season$yday),max(ingest.rate.season$yday)), ylim=c(0,15), xaxt='n', las=1, type="n")
text(min(mean.ingest.rate.season$yday), 14.5,"(b)",cex=1.3)
for (i in 2016:2019) {
  mean.ingest.rate.season.yeari <- calculate.mean.95CI.from.log(ingest.rate.season[ingest.rate.season$year==i,])
  points(mean.ingest.rate.season.yeari$yday, mean.ingest.rate.season.yeari$mean, pch=21, col=c("red","blue","green","orange")[i-2015], cex=0.8)
}
for (i in rev(2016:2019)) lines(ingest.rate.season$yday[ingest.rate.season$year==i], ingest.rate.season$predict.ydaysqxyr.pop[ingest.rate.season$year==i], lwd=3, col=c("red","blue","green","orange")[i-2015]) # to make the distinct seasonal pattern of 2016 best visible
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=c("1 Apr","1 May","1 June","1 July","1 Aug","1 Sep","1 Oct"))
mtext(expression("Prey ingestion rate ( "*min^-1*" )"),2,-1.5,cex=1.2,outer=T)
legend("topright", legend=2016:2019, text.col=c("red","blue","green","orange"), bty="n")
dev.off()
############################### END FIGURE 5 ####################################