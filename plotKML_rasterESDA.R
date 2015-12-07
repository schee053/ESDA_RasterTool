# Purpose        : Create kml file visualization from rasterbrick spacetime object;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Last update    : 04-12-2015
# Note           : Subscript of main_rasterESDA.R


# Set directory
mainDir <- "M:/MyDocuments/ESDA_RasterTool/"
getwd()

# Download and open required packages
require(plotKML)
require(plyr)
require(RSAGA)
require(rgdal)
require(raster)

#-------------------------------------------------------------------------------------------  
# Create KML vector layer of Charge Point locations in 2015
#-------------------------------------------------------------------------------------------
# Tutorial test
# Create bubble out of stations (Value 1, label=Provider?)


ChargeStations <- read.csv("ChargeStations.csv", header = T, sep=";")
View(ChargeStations)
CP_Stations <- ChargeStations[ !duplicated(ChargeStations["CSExternalID"]),]
coordinates(CP_Stations) <- ~Longitude+Latitude
proj4string(CP_Stations) <- CRS("+proj=longlat +datum=WGS84")
str(CP_Stations)
plotKML(CP_Stations["Provider"], colour_scale=rep("#FFFF00", 2), points_names=CP_Stations$Provider) # For color schemes: https://en.wikipedia.org/wiki/Web_colors#HTML_color_names
CP_Essent <- paste(CP_Stations, Provider = "Essent")

# Use altitude parameter (for polygon plotting)
plotKML(NuonClean01["kWh"], altitude=runif(length(NuonClean01))*500)


# With data.frame (creating and plotting a raster)
data(NuonClean01)
gridded(NuonClean01) <- ~Latitude+Longitude
proj4string(NuonClean01) <- CRS("+proj=longlat +datum=WGS84")
data(SAGA_pal) # load list of colors
plotKML(NuonClean01["kWh"], colour_scale=SAGA_pal[[1]])

# Factor type data as raster!!
eberg_grid$LNCCOR6 <- as.factor(paste(eberg_grid$LNCCOR6)) # to make data factor data
levels(eberg_grid$LNCCOR6) # To see factor data
str(eberg_grid$LNCCOR6)
data(worldgrids_pal) # load list of colors
str(worldgrids_pal)
pal = as.character(worldgrids_pal["corine2k"][[1]][c(1,11,13,14,16,17,18)]) #Create characters for legend
pal
plotKML(eberg_grid["LNCCOR6"], colour_scale=pal) # parameter 'colour_scale' automatically adds legend!

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

# Rasterbrick time series
require(raster)
data(LST)
gridded(LST) <- ~lon+lat
proj4string(LST) <- CRS("+proj=longlat +datum=WGS84")
dates <- sapply(strsplit(names(LST), "LST"), function(x){x[[2]]})
datesf <- format(as.Date(dates, "%Y_%m_%d"), "%Y-%m-%dT%H:%M:%SZ")
# begin / end dates +/- 4 days:
TimeSpan.begin = as.POSIXct(unclass(as.POSIXct(datesf))-4*24*60*60, origin="1970-01-01")
TimeSpan.end = as.POSIXct(unclass(as.POSIXct(datesf))+4*24*60*60, origin="1970-01-01")
# pick few climatic stations: (Loop this, so every point will become like this!)
pnts <- HRtemp08[which(HRtemp08$NAME=="Pazin")[1],]
pnts <- rbind(pnts, HRtemp08[which(HRtemp08$NAME=="Crni Lug - NP Risnjak")[1],])
pnts <- rbind(pnts, HRtemp08[which(HRtemp08$NAME=="Cres")[1],])
coordinates(pnts) <- ~Lon + Lat
proj4string(pnts) <- CRS("+proj=longlat +datum=WGS84")
# get the dates from the file names:
LST_ll <- brick(LST[1:5])
LST_ll@title = "Time series of MODIS Land Surface Temperature (8-day mosaics) images"
LST.ts <- new("RasterBrickTimeSeries", variable = "LST", sampled = pnts, rasters = LST_ll, TimeSpan.begin = TimeSpan.begin[1:5], TimeSpan.end = TimeSpan.end[1:5])
data(SAGA_pal)
# plot MODIS images in Google Earth:
plotKML(LST.ts, colour_scale=SAGA_pal[[1]])

# Stations_sp <- SpatialPoints(Stations[,c("Longitude","Latitude")])
# proj4string(Stations_sp) <- CRS("+proj=longlat +datum=WGS84")
# shape <- "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
# kml_open("ChargeStations2015.kml")
# kml_layer(Stations_sp, shape=shape, colour=Stations_sp$Provider)
# kml_legend.bar(Stations_sp)
# kml_close("ChargeStations2015.kml")
# kml_View("ChargeStations2015.kml")

# kml_open("ChargeStations2015.kml")
# kml_layer.SpatialPoints(Stations_sp)
# kml_close("ChargeStations2015.kml")
# kml_View("ChargeStations2015.kml")

#-------------------------------------------------------------------------------------------  
# Create KML vector layer of Charge charge sessions in 2013
#-------------------------------------------------------------------------------------------
sp <- SpatialPoints(NuonClean01[,c("Longitude","Latitude")])
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
hist(dat.xy$kWh) ## log-normal
dat.st <- STIDF(sp, time=dat.xy$Tbegin, data=dat.xy[,c("ID","kWh")], endTime=dat.xy$Tend)
#shape <- "http://maps.google.com/mapfiles/kml/paddle/wht-blank.png"
shape <- "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
kml(NuonClean01[1:5000], colour=log1p(kWh), shape=shape, labels="", kmz=TRUE)
kml_View("NuonJanuari.kmz")

#-------------------------------------------------------------------------------------------  
# Create KML raster layer of Charge Point locations in 2015
#-------------------------------------------------------------------------------------------
# dat.sp <- dat.xy
# coordinates(dat.sp) <- ~ Longitude + Latitude
# proj4string(dat.sp) <- CRS("+proj=longlat +datum=WGS84")
kml_open("ChargeStations2015_raster.kml")
kml_layer.Raster(b2, subfolder.name = "KML", plot.legend = T, metadata = NULL)# gives error: object "kml.out' not found

kml_close("ChargeStations2015_raster.kml")

#-------------------------------------------------------------------------------------------  
# Create KML raster layer with point layer on top
#-------------------------------------------------------------------------------------------

kml_open("Test_RasterPoint.kml")
kml_layer(grid_obj, colour=VariableA)
kml_layer(point_obj, colour=VariableB)
kml_close("Test_RasterPoint.kml")

#-------------------------------------------------------------------------------------------  
# Create KML raster layer with multiple variables
#-------------------------------------------------------------------------------------------
shape <- "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
kml(object, shape=shape, colour=VariableA, labels=VariableB, altitude=VariableB*10, extrude=TRUE)
# Colors to visualize kWh, label and altitude to visualize number of sessions. Or the other way around.
