# only select samples with 32 points (1.6 s), for a fair comparison between the two segmentation methods, as with flexible segmentation, samples < 32 points are kicked out
acc.sample.length.bird.datetime <- aggregate(nobs.segments~date_time+BirdID, df.53.hab.fixed, sum)  
names(acc.sample.length.bird.datetime)[3] <- "acc.sample.length"
df.53.hab.fixed <- merge(df.53.hab.fixed, acc.sample.length.bird.datetime[,1:3], all.x=T)
df.53.hab.fixed <- df.53.hab.fixed[df.53.hab.fixed$acc.sample.length==32,] 

# investigate the proportion of time spent foraging, over the season and in the Waddensea
# calculate proportion of time spent foraging per 24 hours per individual

# proportion of foraging time spent in the Wadden Sea:
df.53.hab.fixed$wadden <- ifelse(is.na(df.53.hab.fixed$Name)==T,0,1)
df.53.hab.fixed$nobs.wadden <- df.53.hab.fixed$wadden*df.53.hab.fixed$nobs.segments
df.foraging <- df.53.hab.fixed[df.53.hab.fixed$foraging==1,]
prop.foraging.wadden <- aggregate(cbind(nobs.wadden,nobs.segments)~year+month+yday+BirdID, df.foraging, sum)
prop.foraging.wadden$prop.for.wadden <- round(prop.foraging.wadden$nobs.wadden/prop.foraging.wadden$nobs.segments,3)

df.53.hab.fixed$ydayz <- (df.53.hab.fixed$yday-mean(df.53.hab.fixed$yday))/sd(df.53.hab.fixed$yday)
df.53.hab.fixed$ydayz_sq <- df.53.hab.fixed$ydayz^2

df.foraging.wadden <- df.53.hab.fixed[is.na(df.53.hab.fixed$Name)==F&df.53.hab.fixed$foraging==1,] # Name is the name of the kombergingsgebied within the Wadden Sea from the Baptist ecotope map

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

### create Table 2 ###
Table2 <- AICc(model.seasonxyr.rnd.slopes, model.season.rnd.slopes, model.season.yr.rnd.slopes, model.yr.rnd.int, model.c.rnd.int)
Table2$dAICc <- Table2$AICc-min(Table2$AICc)
Table2$expmin0.5dAICc <- exp(-0.5*Table2$dAICc)
Table2$Akaikeweight <- round(Table2$expmin0.5dAICc/sum(Table2$expmin0.5dAICc),2)
Table2$dAICc <- round(Table2$dAICc,2) # only round after calculating Akaikeweight
Table2 <- Table2[order(Table2$dAICc),]
row.names(Table2) <- c("season * year","season + year","season","year","-")
write.csv(Table2[,c("df","dAICc","Akaikeweight")], "output/Table2.csv")
write.table(Table2[,c("df","dAICc","Akaikeweight")], "clipboard", sep="\t")
### end of Table 2 ###

### create Table S3 ###
TableS3 <- table(ingest.rate.season$BirdID, ingest.rate.season$month, ingest.rate.season$year) # make a coloured table out of this. 
TableS3 <- cbind(TableS3[,,1],TableS3[,,2],TableS3[,,3],TableS3[,,4])
TOTAL <- colSums(TableS3)
TableS3 <- rbind(TableS3, TOTAL)
write.csv(TableS3, "output/TableS3.csv")
### end of Table S3 ###
