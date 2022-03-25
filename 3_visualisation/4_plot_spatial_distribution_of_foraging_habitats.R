gdata::keep(df.53.hab.fixed, behaviour.pooled, bird.data, sure=T)
source("functions.R")
df.foraging.wadden.fixed <- df.53.hab.fixed[df.53.hab.fixed$wadden==1&df.53.hab.fixed$sex=="F"&df.53.hab.fixed$foraging==1,] 

# the package OpenStreetMap is preferred over ggmap as explained here: https://www.linkedin.com/pulse/plot-over-openstreetmap-ggplot2-abel-tortosa-andreu/
# However, mapping OpenStreetMap maps with ggplot2 is not very convenient
schiermap <- openmap(c(53.525,6.1),c(53.325,6.4),type="bing")
plot(schiermap)
# the openstreetmap is in mercator, translate the map to lat-lon:
map.latlon <- openproj(schiermap, projection = "+proj=longlat")
plot(map.latlon) # gives a very stretched map. Not sure how to solve that.
foragepoints <- data.frame(name=1:3, latitude=c(53.4,53.45,53.5), longitude=c(6.15,6.2,6.25))
# alternative: translate points to mercator:
foragepoints2 <- as.data.frame( 
  OpenStreetMap::projectMercator( lat = foragepoints$latitude, 
                                  long = foragepoints$longitude ) 
)
foragepoints2 <- cbind( foragepoints, foragepoints2)
OpenStreetMap::autoplot.OpenStreetMap( schiermap ) +
  geom_point( data = foragepoints2, aes( x = x, y = y, size = 5 , col="red") ) +
  geom_text( data = foragepoints2, aes( x = x + 100, y = y, label = name ), hjust = 0 ) +
  theme( legend.position = "none" )
# looks much better!!! But is not very intuitive, how to translate points to mercator...

df.foraging.wadden.fixed$lat.rnd <- round(df.foraging.wadden.fixed$latitude,3)
df.foraging.wadden.fixed$lon.rnd <- round(df.foraging.wadden.fixed$longitude,3)
df.ingest.per.coord <- aggregate(ingest~lat.rnd+lon.rnd, df.foraging.wadden.fixed, sum)
df.foraging.per.coord <- aggregate(foraging~lat.rnd+lon.rnd, df.foraging.wadden.fixed, sum) # determine the number of foraging points per rounded latitude/longitude. 
df.ingest.per.coord$ingest_rate <- df.ingest.per.coord$ingest/df.foraging.per.coord$foraging
table(df.ingest.per.coord$ingest_rate)

################
### Figure 5 ###
################
# plot the foraging points of the spoonbills: 
pdf("output/Fig5.pdf", width=10, height=5)
#windows(10,5)
OpenStreetMap::autoplot.OpenStreetMap(map.latlon) +
  geom_tile(data = df.foraging.per.coord, aes( x = lon.rnd, y = lat.rnd, fill=foraging)) +
  scale_fill_gradient(low = "yellow", high = "red", trans="log") +
  xlim(c(6.1,6.35)) +
  ylim(c(53.4,53.5))
dev.off()
### End Figure 5 ###

# plot prey ingestion rates in space
windows(10,5)
OpenStreetMap::autoplot.OpenStreetMap(map.latlon) +
  geom_tile(data = df.ingest.per.coord, aes( x = lon.rnd, y = lat.rnd, fill=ingest_rate)) +
  scale_fill_gradient(low = "yellow", high = "red", trans='sqrt') +
  xlim(c(6.1,6.35)) +
  ylim(c(53.4,53.5))
# now only plot prey ingestion rates when there are at least 30 foraging acc=segments registered for that location
windows(10,5)
OpenStreetMap::autoplot.OpenStreetMap(map.latlon) +
  geom_tile(data = df.ingest.per.coord[df.foraging.per.coord$foraging>30,], aes( x = lon.rnd, y = lat.rnd, fill=ingest_rate)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  xlim(c(6.1,6.35)) +
  ylim(c(53.4,53.5)) 