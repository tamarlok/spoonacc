#################
### Figure S1 ###
#################
# Acceleration patterns during migratory flight of 6291 from Mont St Michel to nearly Marais d'Orx in september 2016
acc.6291 <- acc.6291[order(acc.6291$date.time, acc.6291$Index),]
# only plot selected range of dates, i.e. in this case during migration)
acc.sel <- acc.6291[acc.6291$date.time>="2016-09-25 09:13:07" & acc.6291$date.time<="2016-09-25 14:21:52",] 
pdf("output/FigS1.pdf", width=12, height=7)
layout(matrix(1:32,ncol=8,byrow=T))
par(mar=c(1,1,0,0),oma=c(3.5,3.5,1,1))
count=1
for (i in unique(acc.sel$date.time)) {
  plot(x~Index, acc.sel[acc.sel$date.time==i,], col="red", type="l", xlim=c(1,32), ylim=c(-3,3), xaxt='n', yaxt='n')
  lines(y~Index, acc.sel[acc.sel$date.time==i,], col="blue")
  lines(z~Index, acc.sel[acc.sel$date.time==i,], col="green")
  text(1,-2.8,unique(acc.sel$date.time[acc.sel$date.time==i]), adj=0)
  text(1,2.8,paste("speed =", round(unique(na.omit(acc.sel$speed_2d[acc.sel$date.time==i])),1), "m/s", sep=" "), adj=0)
  if(count<25) axis(1, at=c(0,0.5,1,1.5)*20+1, labels=F) else axis(1, at=c(0,0.5,1,1.5)*20+1, labels=c(0,0.5,1,1.5))
  if(count%in%c(1,9,17,25)) axis(2, at=seq(-3,3,1), labels=T) else axis(2, at=seq(-3,3,1), labels=F)
  count <- count + 1
  if (count==32 | count==length(unique(acc.sel$date.time)))   {
    mtext("Time (s)",1,2,outer=T)
    mtext("acceleration (g)",2,2,outer=T)
  }
}
dev.off()
### END Figure S1 ###

# calculate durations of different behaviours (with max duration at 10 seconds)
acc$date.time.acc <- acc$date.time + seconds(acc$Index/20) # add decimal seconds
strftime(acc$date.time.acc[1], "%Y-%m-%d %H:%M:%OS2")
acc$duration <- NA
acc$duration[1:(length(acc$date.time.acc)-1)] <- acc$date.time.acc[2:length(acc$date.time.acc)]-acc$date.time.acc[1:(length(acc$date.time.acc)-1)]

# change all durations <0.04 s and >0.06 s to NA (these are no sequential behaviours)
acc$duration[acc$duration<0.04|acc$duration>0.06] <- NA # duration should be (close to) 1/20
acc$behav1.seq <- NA
acc$behav1.seq[1] <- 1
# give unique number to each period during which a single behaviour was expressed
# first pool 04.stand-other with 05.stand-shake-feathers
acc$behaviour[acc$behaviour=="stand-shake-feathers"] <- "stand-other"
acc$behaviour1[acc$behaviour1=="05.shake-feathers"] <- "04.stand-other"
acc$behaviour1 <- factor(acc$behaviour1)
for (i in 2:length(acc$date.time.acc)) {
  if (acc$behaviour[i]==acc$behaviour[i-1] & is.na(acc$duration[i])==F) acc$behav1.seq[i]<-acc$behav1.seq[i-1] else acc$behav1.seq[i]<-acc$behav1.seq[i-1]+1
}

duration.behav1.seq <- aggregate(duration~behav1.seq+behaviour1+date.time+BirdID, na.omit(acc), sum)
behaviour1.labels <- substr(levels(duration.behav1.seq$behaviour1),4,30)
observation.bout.lengths <- aggregate(Index~date.time,acc,max)
names(observation.bout.lengths)[2]<- "Index.max"

### TABLE S2 ###
# Table of total duration of annotated behaviours per bird and in total
duration.behav1.bird <- aggregate(duration~behaviour1+BirdID, acc, sum)
duration.behav1.bird <- round(xtabs(duration~behaviour1+BirdID, duration.behav1.bird),0)
duration.behav1.bird <- cbind(duration.behav1.bird, rowSums(duration.behav1.bird))
colnames(duration.behav1.bird)[9]<-"TOTAL"
write.csv(duration.behav1.bird, "output/TableS2.csv")
### END TABLE S2 ###

# Only use the 10 second bouts to show the average lengths of different behaviours (otherwise they may have been cut off at less than 10 seconds)
duration.behav1.seq <- merge(duration.behav1.seq, observation.bout.lengths)
duration.behav1.seq.10 <- duration.behav1.seq[duration.behav1.seq$Index.max==199,] # here, the visually annotated soaring flight bouts are selected out

# Redo the above calculations for the pooled instead of all behaviours (behaviour2)
acc$behav2.seq <- NA
acc$behav2.seq[1] <- 1
for (i in 2:length(acc$date.time.acc)) {
  if (acc$behaviour2[i]==acc$behaviour2[i-1] & is.na(acc$duration[i])==F) acc$behav2.seq[i]<-acc$behav2.seq[i-1] else acc$behav2.seq[i]<-acc$behav2.seq[i-1]+1
}
duration.behav2.seq <- aggregate(duration~behav2.seq+behaviour2+date.time, na.omit(acc), sum)
duration.behav2.seq <- merge(duration.behav2.seq, observation.bout.lengths)
duration.behav2.seq.10 <- duration.behav2.seq[duration.behav2.seq$Index.max==199,]
behaviour2.labels <- substr(levels(acc$behaviour2),4,30)

### Figure S2 ###
pdf("output/FigS2.pdf",width=10,height=5)
layout(matrix(c(rep(1,6),rep(2,4)), nrow=1, byrow=F))
par(mar=c(0,1,0,0), oma=c(10,3,2,1))
# Panel A
boxplot(duration~behaviour1, duration.behav1.seq.10, xlab="", ylab="", xaxt="n", yaxt="n")
axis(1, at=1:length(behaviour1.labels), behaviour1.labels, las=2, cex.axis=1.5)
axis(2, at=seq(0,10,2), cex.axis=1.5, las=1)
mtext("duration (s)", 2, 2.5, cex=1.3, las=0)
axis(3, at=1:13, labels=table(duration.behav1.seq.10$behaviour1), tick=F, line=-0.8, cex.axis=1.5)
axis(3, at=0, labels="N =", tick=F, line=-0.8, cex.axis=1.5)
text(13.5,9.8,"(a)", cex=2)
# Panel B
boxplot(duration~behaviour2, duration.behav2.seq.10, xlab="", ylab="duration (s)", xaxt="n", yaxt="n")
axis(1, at=1:length(behaviour2.labels), behaviour2.labels, las=2, cex.axis=1.5)
axis(3, at=1:length(behaviour2.labels), labels=table(duration.behav2.seq.10$behaviour2), tick=F, line=-0.8, las=1, cex.axis=1.5)
axis(3, at=0.3, labels="N =", tick=F, line=-0.8, las=1, cex.axis=1.5)
text(9.3,9.8,"(b)",cex=2)
dev.off()
### End Figure S2 ###

median(na.omit(duration.behav2.seq.10$duration[duration.behav2.seq.10$behaviour2=="12.for-intake"])) # median duration of ingesting prey is 0.80 s
mean(na.omit(duration.behav2.seq.10$duration[duration.behav2.seq.10$behaviour2=="12.for-intake"]))
# mean duration of ingesting a prey is 0.84 s

# select acceleration samples for plotting representative acceleration samples for the different behaviours
example.stand <- acc[acc$segment.id=="584.1371545958.a",][1:32,]
example.sit <- acc[acc$segment.id=="584.1371545880.a",][1:32,]
example.active.flight <- acc[acc$segment.id=="584.1371546023.a",][1:32,]
example.passive.flight <- acc[acc$segment.id=="6291.1474933495.a",][1:32,]
example.walk <- acc[acc$segment.id=="1608.1435124109.a",][1:32,]
example.search <- acc[acc$segment.id=="1608.1435124018.a",][1:32,] # only searching
example.search.intake <- acc[acc$segment.id=="1608.1435124044.a",][1:32,] # including intake
acc.data=example.search.intake

### FIGURE 2 ###
pdf("output/Fig2.pdf",width=6,height=6)
layout(matrix(1:6, ncol=2, byrow=T))
par(mar=c(1,1,0,0),oma=c(3,3,1,1))
plot.acc.panel(example.stand, plot.y=T, behaviour="stand")
plot.acc.panel(example.sit, behaviour="sit")
plot.acc.panel(example.active.flight, plot.y=T, behaviour="fly (active)")
plot.acc.panel(example.passive.flight, behaviour="fly (passive)")
plot.acc.panel(example.walk, plot.x=T, plot.y=T, behaviour="walk")
plot.acc.panel(example.search.intake, plot.x=T, plot.intake=T, behaviour="search")
mtext("Time (s)", 1, 1.5, outer=T)
mtext("G-force", 2, 1.5, outer=T)
dev.off()
### END FIGURE 2 ###