# Preparation for Figure S7: comparing time allocation and estimated prey ingestion rates for fixed and flexible segmentation
# FIXED
df.foraging.wadden.fixed <- df.53.hab.fixed[df.53.hab.fixed$wadden==1&df.53.hab.fixed$foraging==1,] 
for.suc.yday.fixed <- aggregate(cbind(nobs.foraging,nobs.ingest,foraging)~yday+BirdID+year, df.foraging.wadden.fixed, sum)
# only select estimates based on >20 s acceleration data classified as foraging (where nobs = # seconds x 20 Hz):
for.suc.yday.fixed <- for.suc.yday.fixed[for.suc.yday.fixed$nobs.foraging>20*20,]
for.suc.yday.fixed$ingest.rate <- for.suc.yday.fixed$nobs.ingest/for.suc.yday.fixed$nobs.foraging/0.8*60
mean.for.suc.yday.fixed <- calculate.mean.95CI.from.log(for.suc.yday.fixed)
# FLEXIBLE
df.foraging.wadden.flex <- df.53.hab.flex[df.53.hab.flex$wadden==1&df.53.hab.flex$foraging==1,]
for.suc.yday.flex <- aggregate(cbind(nobs.foraging,nobs.ingest)~yday+BirdID+year, df.foraging.wadden.flex, sum)
for.suc.yday.flex <- for.suc.yday.flex[for.suc.yday.flex$nobs.foraging>20*20,] 
for.suc.yday.flex$ingest.rate <- for.suc.yday.flex$nobs.ingest/for.suc.yday.flex$nobs.foraging/0.8*60
mean.for.suc.yday.flex <- calculate.mean.95CI.from.log(for.suc.yday.flex)

#################
### FIGURE S7 ###
#################
### Comparison of seasonal variation in prey ingestion rates estimated by the fixed versus flexible segmentation method ###
pdf("output/FigS7.pdf",width=9,height=7)
#windows(9,7)
par(mar=c(2,2,1,0), oma=c(2,2,0,1))
# FIXED
plotCI(mean.for.suc.yday.fixed$yday, mean.for.suc.yday.fixed$mean, li=mean.for.suc.yday.fixed$li, ui=mean.for.suc.yday.fixed$ui, xlab="", ylab = "", ylim=c(0,15), xlim=c(min(mean.for.suc.yday.fixed$yday),max(mean.for.suc.yday.fixed$yday)), pch=21, sfrac=0, gap=0, pt.bg="white", col="steelblue2", xaxt='n', las=1)
# FLEXIBLE
plotCI(mean.for.suc.yday.flex$yday+0.5, mean.for.suc.yday.flex$mean, li=mean.for.suc.yday.flex$li, ui=mean.for.suc.yday.flex$ui, add=T, pch=21, sfrac=0, gap=0, col="orange2", pt.bg="white", xaxt='n', ylim=c(0,0.3))
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=c("1 Apr","1 May","1 June","1 July","1 Aug","1 Sep","1 Oct"))
mtext(expression("Prey ingestion rate ( "*min^-1*" )"),2,2.5,cex=1.2)
dev.off()
### END FIGURE S7 ###

median((mean.for.suc.yday.fixed$mean/mean.for.suc.yday.flex$mean)) # ratio 1.38
median((mean.for.suc.yday.flex$mean/mean.for.suc.yday.fixed$mean)) # ratio 0.73
median((mean.for.suc.yday.fixed$mean-mean.for.suc.yday.flex$mean)/mean(c(mean.for.suc.yday.fixed$mean,mean.for.suc.yday.flex$mean))) # 33% higher estimated prey ingestion rates using fixed compared to flexible segmentation method

# Prepare data for the map with the foraging locations (Figure 4)
# the package OpenStreetMap is preferred over ggmap as explained here: https://www.linkedin.com/pulse/plot-over-openstreetmap-ggplot2-abel-tortosa-andreu/
schiermap <- openmap(c(53.525,6.1),c(53.325,6.4),type="bing")
df.foraging.wadden.fixed$lat.rnd <- round(df.foraging.wadden.fixed$latitude,3)
df.foraging.wadden.fixed$lon.rnd <- round(df.foraging.wadden.fixed$longitude,3)
df.foraging.per.coord <- aggregate(foraging~lat.rnd+lon.rnd, df.foraging.wadden.fixed, sum) # determine the number of foraging points per rounded latitude/longitude. 

# transpose the foraging points to mercator projection
df.foraging.per.coord.merc <- as.data.frame( 
  OpenStreetMap::projectMercator( lat = df.foraging.per.coord$lat.rnd, 
                                  long = df.foraging.per.coord$lon.rnd ) 
)
df.foraging.per.coord.merc <- cbind( df.foraging.per.coord, df.foraging.per.coord.merc)

# get mercator locations for axes:
grid.locations <- expand.grid(longitude=c(6.1,6.2,6.3,6.4), latitude=c(53.4,53.42,53.44,53.46,53.48,53.50))
grid.locations.merc <- as.data.frame( 
  OpenStreetMap::projectMercator( lat = grid.locations$latitude, 
                                  long = grid.locations$longitude ) 
)
grid.locations.merc <- cbind( grid.locations, grid.locations.merc)
grid.x <- unique(grid.locations.merc[,c("x","longitude")])
grid.y <- unique(grid.locations.merc[,c("y","latitude")])
map.NL <- readPNG("data/raw/NL_greyscale_with_Schier.png", native=T)

################
### FIGURE 4 ###
################
# plot the density of foraging locations of the spoonbills 
pdf("output/Fig4.pdf", width=11, height=5)
#windows(10,5)
OpenStreetMap::autoplot.OpenStreetMap(schiermap) + 
  geom_tile(data = df.foraging.per.coord.merc, aes( x = x, y = y, fill=foraging)) +
  theme(legend.position = c(1.1,0.8)) +
  scale_fill_gradient(low = "yellow", high = "red", trans="log", labels=c(1,7,55,403,2981), name="density") + 
  scale_x_continuous("Longitude (°E)", breaks=grid.x$x, labels=grid.x$longitude, limits=c(679000,712600)) +
  scale_y_continuous("Latitude (°N)", breaks=grid.y$y, labels=format(grid.y$latitude,digits=4), limits=c(7055000,7076000)) +
  inset_element(map.NL, left=1, bottom=0, right=1.2, top=0.4)
dev.off()
### END FIGURE 4 ###

################
### FIGURE 5 ###
################
### Seasonal variation in prey ingestion rates using fixed segmentation method ###
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
### END FIGURE 5 ###

# Data preparation for Figure S9
# overall time budget fixed segmentation; to have sufficient data per day, only select months April-September:
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
### Figure S9 ###
#################
### proportion of time spent foraging and ingesting and the subsequent prey ingestion rate estimated by the two segmentation methods, using either all data or only data in the Wadden Sea ###
pdf("output/FigS9.pdf", width=10, height=7)
#windows(10,7)
# all data
layout(matrix(1:6,ncol=2,byrow=F))
par(mar=c(1,5,0,0), oma=c(1,0,3,1), mgp=c(3.5,1,0))
plot(prop.search~yday, prop.foraging.day.flex, ylab="Perc. time searching", pch=21, col="orange2", xaxt="n", yaxt="n", ylim=c(0.15,0.55), cex.lab=1.5)
mtext("All data",3,1,cex=1.3)
points(prop.search~yday, prop.foraging.day.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(2, at=seq(0,0.5,0.1), labels=paste(seq(0,50,10),"%",sep=""), line=0, las=1, cex.axis=1.3)
legend("bottomleft", c("fixed","flexible"), pch=21, col=c("steelblue2","orange2"), bty="n", cex=1.5)
plot(prop.ingest~yday, prop.foraging.day.flex, ylab="Perc. time ingesting", pch=21, col="orange2", xaxt="n", yaxt="n", las=1, ylim=c(0,0.06), cex.lab=1.5)
points(prop.ingest~yday, prop.foraging.day.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(2, at=seq(0,0.06,0.01), labels=paste(seq(0,6,1),"%",sep=""), line=0, las=1, cex.axis=1.3)
plot(ingest.rate~yday, prop.foraging.day.flex, ylab="", pch=21, col="orange2", ylim=c(0,11), xaxt="n", las=1, cex.lab=1.5, cex.axis=1.3)
mtext(expression("Prey ingestion rate ( "*min^-1*" )"),2,3)
points(ingest.rate~yday, prop.foraging.day.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1)+15), labels=c("Apr","May","June","July","Aug","Sep"), tick=F, line=-0.5, cex.axis=1.3)
# Wadden Sea data:
plot(prop.search~yday, prop.foraging.day.wadden.flex, ylab="", pch=21, col="orange2", xaxt="n", yaxt="n", ylim=c(0.25,0.80), cex.lab=1.5)
mtext("Wadden Sea data",3,1,cex=1.3)
points(prop.search~yday, prop.foraging.day.wadden.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(2, at=seq(0,1,0.1), labels=paste(seq(0,100,10),"%",sep=""), line=0, las=1, cex.axis=1.3)
plot(prop.ingest~yday, prop.foraging.day.wadden.flex, ylab="", pch=21, col="orange2", xaxt="n", yaxt="n", las=1, ylim=c(0,0.12), cex.lab=1.5)
points(prop.ingest~yday, prop.foraging.day.wadden.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(2, at=seq(0,0.12,0.02), labels=paste(seq(0,12,2),"%",sep=""), line=0, las=1, cex.axis=1.3)
plot(ingest.rate~yday, prop.foraging.day.wadden.flex, ylab="", pch=21, col="orange2", ylim=c(0,12), xaxt="n", las=1, cex.lab=1.5, cex.axis=1.3)
points(ingest.rate~yday, prop.foraging.day.wadden.fixed, pch=21, col="steelblue2")
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1,30+31+30+31+31+30+1)), labels=F)
axis(1, at=c(31+28+31+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1)+15), labels=c("Apr","May","June","July","Aug","Sep"), tick=F, line=-0.5, cex.axis=1.3)
dev.off()
### END FIGURE S9 ### 

# Data preparation for Figure S10
# calculate time budgets for fixed and flexible segmentation in table format:
dur.behav.yday.fixed.xtabs = xtabs(nobs.segments~pred.behav+yday, data=df.53.hab.fixed[df.53.hab.fixed$month%in%4:9,])
prop.behav.yday.fixed <- prop.table(dur.behav.yday.fixed.xtabs,2)
dur.behav.yday.flex.xtabs = xtabs(nobs.segments~pred.behav+yday, data=df.53.hab.flex[df.53.hab.flex$month%in%4:9,])
prop.behav.yday.flex <- prop.table(dur.behav.yday.flex.xtabs,2)

##################
### FIGURE S10 ###
##################
pdf("output/FigS10.pdf", width=10, height=4)
#windows(10,4)
layout(matrix(1:2, ncol=2))
par(mar=c(1,1,2,0), oma=c(1,3,0,8))
bardata <- barplot(prop.behav.yday.fixed[behaviour.pooled.ordered,], main='', col=behaviour.colors, las=1, xaxt='n', yaxt='n', border=NA, ylab="", space=0)
axis(1, at=bardata[15+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1)], labels=c("Apr","May","June","July","Aug","Sep"), tick=F, line=-1)
axis(2, at=seq(0,1,0.2), labels=paste(seq(0,100,20),"%",sep=""), line=-0.5, las=1)
mtext('Percentage of time', 2, 3, cex=1.2)
mtext('FIXED', 3, 0.6, cex=1)
barplot(prop.behav.yday.flex[behaviour.pooled.ordered,], main='', col=behaviour.colors, las=1, xaxt='n', yaxt="n", border=NA, space=0)
mtext('FLEXIBLE', 3, 0.6, cex=1)
legend(190, 0.6, rev(behaviour.labels), pch=15, col=rev(behaviour.colors), bty='n', xpd=NA, cex=1)
axis(1, at=bardata[15+c(1,30+1,30+31+1,30+31+30+1,30+31+30+31+1,30+31+30+31+31+1)], labels=c("Apr","May","June","July","Aug","Sep"), tick=F, line=-1)
dev.off()
### END FIGURE S10 ###