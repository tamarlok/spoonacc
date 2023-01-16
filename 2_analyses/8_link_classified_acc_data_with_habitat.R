# only select GPS data, as these contain the unique GPS locations with which to link habitat information
# these GPS data are the same in df.all.fixed.0.4 and df.all.flex
# make unique BirdID, datetime and location df to link tide and daynight and habitat to:
df.uni <- unique(df.all.fixed.0.4[,c("BirdID","date_time","latitude","longitude")])

# first only select data >53N, so that below calculations are faster
df.uni$year <- year(df.uni$date_time)
df.uni$month <- month(df.uni$date_time)
df.uni.53 <- df.uni[df.uni$latitude>53,]

# Import the tidal basin data from the Baptist ecotope map downloaded from https://doi.org/10.17632/27mysx289g 
tidalbasinsWZ <- readOGR("data/raw/Ecotope Wadden Sea - Baptist et al 2019", layer="tidalbasins_ETRS_1989_UTM32")
# transform to WGS84 with epsg=4326 to have it in the same projection as the GPS-data
tidalbasinsWZ_WGS84 <- spTransform(tidalbasinsWZ, CRS=CRS("+init=epsg:4326"))
proj4string(tidalbasinsWZ_WGS84) # check projection
# give the same projection to df.uni.53
coordinates(df.uni.53)  <- ~longitude + latitude  # to make it a spatialpoints dataframe
proj4string(df.uni.53) <- CRS("+init=epsg:4326")  # to set crs = wgs
# link Baptist tidal basins to existing file 
tidalbasin.data.to.add <- over(df.uni.53, tidalbasinsWZ_WGS84) 
df.uni.53 <- data.frame(df.uni.53, tidalbasin.data.to.add)

# link the habitat data to all classified GPS/ACC data
df.53.hab.fixed <- add.columns.to.application.data(df.all.fixed.0.4)
df.53.hab.flex <- add.columns.to.application.data(df.all.flex)

gdata::keep(df.all.fixed.0.4, df.all.flex, RF.fixed.0.4, RF.flexible.100, bird.data, behaviour.colors, behaviours, sure=T)
