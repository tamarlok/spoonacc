# only select GPS data, as these contain the unique GPS locations with which to link habitat information (which makes things ca. 4x faster)
# these GPS data are the same in df.all.fixed.0.4 and df.all.flex
# make unique BirdID, datetime and location df to link tide and daynight and habitat to:
df.uni <- unique(df.all.fixed.0.4[,c("BirdID","date_time","latitude","longitude")])

# first only select data >53N, so that below calculations are faster
df.uni$year <- year(df.uni$date_time)
df.uni$month <- month(df.uni$date_time)
df.uni.53 <- df.uni[df.uni$latitude>53,]

# Import Baptist shapefiles
# import the tidal basin shapefile
tidalbasinsWZ <- readOGR("data/raw/Ecotope Wadden Sea - Baptist et al 2019", layer="tidalbasins_ETRS_1989_UTM32")
proj4string(tidalbasinsWZ)
windows()
plot(tidalbasinsWZ)
# transform to WGS84 with epsg=4326 to have it in the same projection as the GPS-data
tidalbasinsWZ_WGS84 <- spTransform(tidalbasinsWZ, CRS=CRS("+init=epsg:4326"))
proj4string(tidalbasinsWZ_WGS84)
# give the same projection to df.uni.53
coordinates(df.uni.53)  <- ~longitude + latitude  # to make it a spatialpoints dataframe
proj4string(df.uni.53) <- CRS("+init=epsg:4326")  # to set crs = wgs
# link Baptist tidal basins to existing file 
tidalbasin.data.to.add <- over(df.uni.53, tidalbasinsWZ_WGS84) 
df.uni.53 <- data.frame(df.uni.53, tidalbasin.data.to.add)

# link to Schier map
load('data/raw/ShapeFileSchier.RData')
coordinates(df.uni.53)  <- ~longitude + latitude  # to make it a spatialpoints dataframe
proj4string(df.uni.53) <- CRS("+init=epsg:4326")  # to give the correct projection
proj4string(schier_new84_sel)
habitats.to.add <- over(df.uni.53, schier_new84_sel) 
# group habitats
habitats.to.add$habitat = as.character(habitats.to.add$Habitat)
habitats.to.add$habitat[is.na(habitats.to.add$habitat)] <- "unknown"
habitats.to.add$habitat[habitats.to.add$habitat=="Wadgeulen_Diep"|habitats.to.add$habitat=="Wadgeulen_Ondiep"|habitats.to.add$habitat=="Wadplaten"|habitats.to.add$habitat=="Wad_Kweldergeul_Brak"]="waddenzee" 
habitats.to.add$habitat[habitats.to.add$habitat=="Schier_Kweldergeul_Brak"|habitats.to.add$habitat=="Schier_Brak_Rest"]="Schier_brak"
habitats.to.add$habitat[habitats.to.add$habitat=="Wal_Zoet_Ondiep"|habitats.to.add$habitat=="Wal_Zoet_Diep"]="wal_rest_zoet"
habitats.to.add$habitat[habitats.to.add$habitat=="Wal_Kwelder"|habitats.to.add$habitat=="Wal_Land_Rest"|habitats.to.add$habitat=="Wal_Moeras"]="wal_rest_land"
habitats.to.add$habitat[habitats.to.add$habitat=="LG_Land_Rest"|habitats.to.add$habitat=="LG_Moeras"]="LM_land"
habitats.to.add$habitat[habitats.to.add$habitat=="LG_Zoet_Ondiep"|habitats.to.add$habitat=="LG_Zoet_Diep"]="LM_zoet"
df.uni.53 <- data.frame(df.uni.53, habitats.to.add)

# link the habitat data to all classified acc data
df.53.hab.fixed <- add.columns.to.application.data(df.all.fixed.0.4)
df.53.hab.flex <- add.columns.to.application.data(df.all.flex)
rm(schier_new84_sel)
