gdata::keep(df.53.hab.fixed, bird.data, sure=T)
source("functions.R")
df.foraging.wadden.fixed <- df.53.hab.fixed[df.53.hab.fixed$wadden==1&df.53.hab.fixed$sex=="F"&df.53.hab.fixed$foraging==1,] 

# the package OpenStreetMap is preferred over ggmap as explained here: https://www.linkedin.com/pulse/plot-over-openstreetmap-ggplot2-abel-tortosa-andreu/
# However, mapping OpenStreetMap maps with ggplot2 is not very convenient
schiermap <- openmap(c(53.525,6.1),c(53.325,6.4),type="bing")
schiermap.zoom <- openmap(c(53.47,6.22),c(53.46,6.24),type="bing")
OpenStreetMap::autoplot.OpenStreetMap(schiermap.zoom)
squarepoints <- data.frame(name=1:4, latitude=c(53.4608,53.4608,53.4673,53.4673), longitude=c(6.2280,6.2388,6.2280,6.2388))
squarepoints2 <- as.data.frame( 
  OpenStreetMap::projectMercator( lat = squarepoints$latitude, 
                                  long = squarepoints$longitude ) 
)
squarepoints2 <- cbind( squarepoints, squarepoints2)
windows()
OpenStreetMap::autoplot.OpenStreetMap(schiermap.zoom) +
  geom_point( data = squarepoints2, aes( x = x, y = y, col="red") )
# to get the same distance on latitude and longitude axis, one would need 64 units latitude against 108 units longitude (I measured this on Google Earth)
# to keep things simple, I will stick to rounding both latitude and longitude to 3 decimals, which results in rectangles rather than squares though... 

windows()
OpenStreetMap::autoplot.OpenStreetMap(schiermap)
# the openstreetmap is in mercator, translate the map to lat-lon:
map.latlon <- OpenStreetMap::openproj(schiermap, projection = "+proj=longlat")
plot(map.latlon) # gives a very stretched map. Not sure how to solve that.
foragepoints <- data.frame(name=1:3, latitude=c(53.4,53.45,53.5), longitude=c(6.15,6.2,6.25))
OpenStreetMap::autoplot.OpenStreetMap( map.latlon ) +
  geom_point( data = foragepoints, aes( x = longitude, y = latitude, size = 5 , col="red") )

# alternative: translate points to mercator while keeping the map in mercator projection:
foragepoints2 <- as.data.frame( 
  OpenStreetMap::projectMercator( lat = foragepoints$latitude, 
                                  long = foragepoints$longitude ) 
)
foragepoints2 <- cbind( foragepoints, foragepoints2)
windows()
OpenStreetMap::autoplot.OpenStreetMap( schiermap ) +
  geom_point( data = foragepoints2, aes( x = x, y = y, size = 5 , col="red") ) +
  geom_text( data = foragepoints2, aes( x = x + 100, y = y, label = name ), hjust = 0 ) +
  theme( legend.position = "none" )
# now the projection is correct, of both map and forage points

df.foraging.wadden.fixed$lat.rnd <- round(df.foraging.wadden.fixed$latitude,3)
df.foraging.wadden.fixed$lon.rnd <- round(df.foraging.wadden.fixed$longitude,3)
df.ingest.per.coord <- aggregate(ingest~lat.rnd+lon.rnd, df.foraging.wadden.fixed, sum)
df.foraging.per.coord <- aggregate(foraging~lat.rnd+lon.rnd, df.foraging.wadden.fixed, sum) # determine the number of foraging points per rounded latitude/longitude. 
df.ingest.per.coord$ingest_rate <- df.ingest.per.coord$ingest/df.foraging.per.coord$foraging
table(df.ingest.per.coord$ingest_rate)

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

windows(10,5)
OpenStreetMap::autoplot.OpenStreetMap(schiermap) + 
  geom_tile(data = df.foraging.per.coord.merc, aes( x = x, y = y, fill=foraging)) +
  theme(legend.position = c(1.1,0.8)) +
  scale_fill_gradient(low = "yellow", high = "red", trans="log", labels=c(1,7,55,403,2981)) + 
  scale_x_continuous("Longitude (°E)", breaks=grid.x$x, labels=grid.x$longitude, limits=c(679000,712600)) +
  scale_y_continuous("Latitude (°N)", breaks=grid.y$y, labels=format(grid.y$latitude,digits=4), limits=c(7055000,7076000)) +
  inset_element(map.NL, left=1, bottom=0, right=1.2, top=0.4)

################
### Figure 4 ###
################
# plot the foraging points of the spoonbills: 
pdf("output/Fig4.pdf", width=11, height=5)
#windows(10,5)
OpenStreetMap::autoplot.OpenStreetMap(schiermap) + 
  geom_tile(data = df.foraging.per.coord.merc, aes( x = x, y = y, fill=foraging)) +
  theme(legend.position = c(1.1,0.8)) +
  scale_fill_gradient(low = "yellow", high = "red", trans="log", labels=c(1,7,55,403,2981)) + 
  scale_x_continuous("Longitude (°E)", breaks=grid.x$x, labels=grid.x$longitude, limits=c(679000,712600)) +
  scale_y_continuous("Latitude (°N)", breaks=grid.y$y, labels=format(grid.y$latitude,digits=4), limits=c(7055000,7076000)) +
  inset_element(map.NL, left=1, bottom=0, right=1.2, top=0.4)
dev.off()
### End Figure 4 ###

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
