# Purpose        : Create kml file visualization from rasterbrick spacetime object;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Last update    : 04-12-2015
# Note           : Subscript of main_rasterESDA.R, make sure "preProcessing_rasterESDA.R" script has run, before starting this script.


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
# Questions about plotKML:
#-------------------------------------------------------------------------------------------


# How to add a legend in "plotKML" (instead of kml_layer)?
# How to to make same color legend and map in kml_layer/png (see bellow)?
# It is possible to plot multiple layers in one kml. Also with "plotKML"?
# What is the difference between KML, plotKML and kml_layer
  # e.g. line 74 (plot NUON) only works with KML, and not plotKML...?


#-------------------------------------------------------------------------------------------  
# Create KML vector layer of Charge Point locations in 2015
#-------------------------------------------------------------------------------------------
kml_stations <- function (csv.name, shape, kml.name, legend=TRUE, balloon = TRUE){
  obj <- read.csv(csv.name, header = T, sep=";")
  obj$Address <- paste(obj$Street, obj$HouseNumber, sep=" ")
  obj <- obj[ !duplicated(obj["CSExternalID"]),]
  coordinates(obj) <- ~Longitude+Latitude
  proj4string(obj) <- CRS("+proj=longlat +datum=WGS84")
  # Remove unnecessary collomn
  obj_keep <- c("CPExternalID", "Street", "HouseNumber", "PostalCode", "City", "Provider", "VehicleType", "Address")
  obj <- obj[obj_keep] 
  
  shape <- shape
  kml_open(kml.name)
  kml_legend.bar(obj$Provider, legend.pal=SAGA_pal[[1]][c(1,20)], legend.file = "Providers.png") 
  kml_screen(image.file = "Providers.png", position = "UL", sname = "Providers")
  kml_layer(obj[c("Provider","Address")], shape = shape, LabelScale =.5, colour=Provider, colour_scale=SAGA_pal[[1]], points_names="", balloon=T)
  kml_close(kml.name)
  kml_View(kml.name)
}

#kml_stations("ChargeStations.csv", "http://maps.google.com/mapfiles/kml/paddle/wht-blank.png", "Stations2015.kml", legend=TRUE, balloon=TRUE)
#------------------------------------------------------------------------------------------- 
# Create KML vector layer of Charge Point locations in 2013
#-------------------------------------------------------------------------------------------
NuonClean01$LonLat <- paste(NuonClean01$Longitude, NuonClean01$Latitude)
EssentClean06$LonLat < paste(EssentClean06$Longitude, EssentClean01$Latitude)
Stations$LonLat < paste(Stations$Longitude, Stations$Latitude) ## ERROR: logical(0)

NuonXYunique <- NuonClean01[ !duplicated(NuonClean01["LonLat"]),]
EssentXYunique <- EssentClean06[ !duplicated(EssentClean06["LonLat"]),]
XYunique <- rbind(NuonXYunique, EssentXYunique)

Stations2013 <- join(XYunique, Stations, by = LonLat , type = "left", match = "first")

#-------------------------------------------------------------------------------------------  
# Create STIDF from Nuon Januari 2013
#-------------------------------------------------------------------------------------------

NuonClean01$Address <- paste(NuonClean01$Street, NuonClean01$HouseNumber, sep="_")
CP_NUON01 <- SpatialPoints(NuonClean01[,c("Longitude","Latitude")])
proj4string(CP_NUON01) <- CRS("+proj=longlat +datum=WGS84")
hist(NuonClean01$kWh)
CP_NUON01.st <- STIDF(CP_NUON01, time=NuonClean01$BEGIN_CS, data=NuonClean01[,c("Address", "Provider","kWh")], endTime=NuonClean01$END_CS)
View(CP_NUON01.st)

#-------------------------------------------------------------------------------------------  
# Create STIDF from Nuon June 2013
#-------------------------------------------------------------------------------------------
# Possible arguments KML function: http://rgm.ogalab.net/RGM/R_rdfile?f=plotKML/man/plotKML.Rd&d=R_CC 
# http://plotkml.r-forge.r-project.org/plotKML.html

NuonClean06$Address <- paste(NuonClean06$Street, NuonClean06$HouseNumber, sep="_")
CP_NUON06 <- SpatialPoints(NuonClean06[,c("Longitude","Latitude")])
proj4string(CP_NUON06) <- CRS("+proj=longlat +datum=WGS84")
hist(NuonClean06$kWh)
CP_NUON06.st <- STIDF(CP_NUON06, time=NuonClean06$BEGIN_CS, data=NuonClean06[,c("Address", "Provider","kWh")], endTime=NuonClean06$END_CS)
View(CP_NUON06.st)

#-------------------------------------------------------------------------------------------  
# Plot STIDF in hight from Nuon Januari 2013
#-------------------------------------------------------------------------------------------

kml(CP_NUON01.st[1:7378], colour=log1p(kWh), shape=shape, labels="", kmz=F, balloon=T)

Session_vertical <- function (obj, kml.name){ 
  obj.sp <- obj
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  
  kml_open(kml.name)
  kml_layer.SpatialPoints(obj.sp[c("kWh","Address", "Provider")], TimeSpan.begin=format(obj.sp$BEGIN_CS, "%Y-%m-%dT%H:%M:%SZ"), TimeSpan.end=format(obj.sp$END_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh*10, colour=log1p(kWh), colour_scale=R_pal[["heat_colors"]], shape=shape, labels="", altitudeMode="relativeToGround", balloon = TRUE)
  kml_close(kml.name)
  kml_View(kml.name)
} 

Session_vertical(NuonClean01, "NuonJanuari2013.kml")

## Spacetime density:
library(spatstat)
NuonClean01$t <- as.integer((unclass(NuonClean01$BEGIN_CS) + unclass(NuonClean01$END_CS))/2)
NuonClean01.ppx <- ppx(data=NuonClean01[,c("Longitude","Latitude","t","kWh")], coord.type=c("s","s","t","m"))
int <- intensity(NuonClean01.ppx)

#kml.tiles(obj=NuonClean01.sp, folder.name="NuonClean", file.name="NuonClean01_tiled.kml", block.x=0.05, cpus=10, home.url=".", desc="NuonClean", open.kml=TRUE, return.list=FALSE, TimeSpan.begin=format(NuonClean01.sp$BEGIN_CS, "%Y-%m-%dT%H:%M:%SZ"), TimeSpan.end=format(NuonClean01.sp$END_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh*10, colour=log1p(kWh), shape=shape, labels=kWh, altitudeMode="relativeToGround")

#-------------------------------------------------------------------------------------------  
# Plot STIDF in hight from Nuon June 2013
#-------------------------------------------------------------------------------------------

NuonClean06.sp <- NuonClean06
coordinates(NuonClean06.sp) <- ~ Longitude + Latitude
proj4string(NuonClean06.sp) <- CRS("+proj=longlat +datum=WGS84")
kml_open("NuonClean06_High.kml")
kml_layer.SpatialPoints(NuonClean06.sp, TimeSpan.begin=format(NuonClean06.sp$BEGIN_CS, "%Y-%m-%dT%H:%M:%SZ"), TimeSpan.end=format(NuonClean06.sp$END_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh*10, colour=log1p(kWh), shape=shape, labels=kWh, altitudeMode="relativeToGround")
kml_close("NuonClean06_High.kml")
kml_View("NuonClean06_High.kml")




















## From tutorial:


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
