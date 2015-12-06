# Purpose        : Run the tutorials of plotKML on http://gsif.isric.org/doku.php?id=wiki:tutorial_plotkml;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Last update    : 06-12-2015
# Note           : 

require(plotKML)
require(sp)
require(raster)

getwd()
mainDir <- "E:/"
subDir <- "Tutorial_plotKML"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))
getwd()

# Point data.frame
data(eberg)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
eberg <- eberg[runif(nrow(eberg))<.2,]
bubble(eberg["CLYMHT_A"])
plotKML(eberg["CLYMHT_A"],  colour_scale=rep("#00FF00", 2), points_names="")

# Spatial lines data.frame
data(eberg_contours)
plotKML(eberg_contours)
str(eberg_contours)

# SpatialPolygons data.frame
data(eberg_zones)
View(eberg_zones) # No spatial column! Internal coordinates?
length(eberg_zones) # Number of rows...
plotKML(eberg_zones["ZONES"])
plotKML(eberg_zones["ZONES"], altitude=runif(length(eberg_zones))*500)

# RasterLayer data.frame
data(eberg_grid)
str(eberg_grid)
View(eberg_grid)
gridded(eberg_grid) <-  ~x+y
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
data(SAGA_pal)
str(SAGA_pal)
plotKML(eberg_grid["TWISRT6"], colour_scale= SAGA_pal[[1]])

# Factor type data as raster!!
eberg_grid$LNCCOR6 <- as.factor(paste(eberg_grid$LNCCOR6))
levels(eberg_grid$LNCCOR6)
str(eberg_grid$LNCCOR6)
data(worldgrids_pal)
str(worldgrids_pal)
pal = as.character(worldgrids_pal["corine2k"][[1]][c(1,11,13,14,16,17,18)]) #Create characters for legend
pal
plotKML(eberg_grid["LNCCOR6"], colour_scale=pal) # parameter 'colour_scale' automatically adds legend! But needs to be a color variable. 
                                                  #Not just any number.

# Space-Time Irregular DataFrame (STIDF) 
data(HRtemp08)
HRtemp08$ctime <- as.POSIXct(HRtemp08$DATE, format="%Y-%m-%dT%H:%M:%SZ")
library(spacetime)
sp <- SpatialPoints(HRtemp08[,c("Lon", "Lat")])
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
HRtemp08.st <- STIDF(sp, time = HRtemp08$ctime, data = HRtemp08[,c("NAME","TEMP")])
HRtemp08_jan <- HRtemp08.st[1:500]
str(HRtemp08_jan)
plotKML(HRtemp08_jan[,,"TEMP"], dtime = 24*3600) # shows TEMP variable every minute

# Spatial trajectories
data(gpxbtour)
View(gpxbtour)
gpxbtour$ctime <- as.POSIXct(gpxbtour$time, format=("%Y-%m-%dT%H:%M:%SZ"))
coordinates(gpxbtour) <- ~lon+lat
proj4string(gpxbtour) <- CRS("+proj=longlat +datum=WGS84")
install.packages("fossil")
require(fossil)
xy <- as.list(data.frame(t(coordinates(gpxbtour))))
gpxbtour$dist.km <- sapply(xy, function(x) { deg.dist(long1=x[1], lat1=x[2], long2=xy[[1]][1], lat2=xy[[1]][2]) } )
library(spacetime)
install.packages("adehabitat")
library(adehabitat)
gpx.ltraj <- as.ltraj(coordinates(gpxbtour), gpxbtour$ctime, id = "th")
gpx.st <- as(gpx.ltraj, "STTDF")
gpx.st$speed <- gpxbtour$speed
gpx.st@sp@proj4string <- CRS("+proj=longlat +datum=WGS84")
str(gpx.st)
plotKML(gpx.st, colour="speed") # Gives speed color

# Spatial Metadata # Doesn't work yet! 
# first: argument out.xml.file is missing with no default 
# second: object Target_variable not found
eberg.md <- spMetadata(eberg, xml.file=system.file("eberg.xml", package="plotKML"), out.xml.file="Metadata", Target_variable="SNDMHT_A")
str(eberg.md)
plotKML(eberg["CLYMHT_A"], metadata=eberg.md)

# Rasterbrick time series



# Work with Remote Sensing data
imagename = "Soil_monolith.jpg"
x1 <- getWikiMedia.ImageInfo(imagename) # Upload your photo that can be georefferenced on WikiMedia
sm <- spPhoto(filename = x1$url$url, exif.info= x1$metadata)
str(sm)
plotKML(sm)

# 3D soil profile
require(aqp)
require(plyr)
lon = 3.90; lat = 7.50; id = "ISRIC:NG0017"; FAO1988 = "LXp"
top = c(0, 18, 36, 65, 87, 127)
bottom = c(18, 36, 65, 87, 127, 181)
ORCDRC = c(18.4, 4.4, 3.6, 3.6, 3.2, 1.2)
hue = c("7.5YR", "7.5YR", "2.5YR", "5YR", "5YR", "10YR")
value = c(3, 4, 5, 5, 5, 7); chroma = c(2, 4, 6, 8, 4, 3)
prof1 <- join(data.frame(id, top, bottom, ORCDRC, hue, value, chroma), data.frame(id, lon, lat, FAO1988), type='inner')
prof1$soil_color <- with(prof1, munsell2rgb(hue, value, chroma))
depths(prof1) <- id ~ top + bottom
site(prof1) <- ~ lon + lat + FAO1988
coordinates(prof1) <- ~ lon + lat
proj4string(prof1) <- CRS("+proj=longlat +datum=WGS84")
prof1
plotKML(prof1, var.name="ORCDRC", color.name="soil_color")
