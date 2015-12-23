# Split (subset) Nuon files
EssentTest <- read.csv("exp_201301-62014.csv", header = T, sep=",")
View(EssentTest)
EssentTest$BEGIN_LOAD_DATE <- as.POSIXct(paste(EssentTest$BEGIN_LOAD_DATE), format="%d.%m.%Y", tz = "GMT")
EssentTest2013 <- subset(EssentTest, BEGIN_LOAD_DATE < as.POSIXct("2013-01-05 00:00"))
EssentTest2013 <- subset(EssentTest2013, BEGIN_LOAD_DATE > as.POSIXct("2013-01-01 00:00"))
View(EssentTest2013)
#write.csv(EssentTest2013, file= paste("EssentTest2013", "csv", sep = "."))

if (!require(plyr)) install.packages('plyr')
EssentTest2013$kWh <- as.numeric(EssentTest2013$ENERGIE)
EssentTest2013$kWh2 <- as.character(EssentTest2013$ENERGIE)
EssentTest2013$kWh3 <- as.numeric(EssentTest2013$kWh2)

EssentTest2013$kWh2 <- gsub(",", "", EssentTest2013$kWh2, fixed = TRUE)
EssentTest2013$kWh3 <- as.numeric(EssentTest2013$kWh2)
EssentTest2013$kWh4 <- (EssentTest2013$kWh3/10000)

EssentTest2013$Weekday <- weekdays(as.Date(EssentTest2013$BEGIN_LOAD_DATE, format = '%Y-%m-%d', tz = "GMT"))
EssentTest2013$CHARGE_TIME <- as.character(EssentTest2013$CHARGE_DURATION)
Res <- as.POSIXct(EssentTest2013$CHARGE_TIME, format = "%H:%M:%S", tz = "GMT")
install.packages("chron")
library(chron)

EssentTest2013$ch <- times(EssentTest2013$CHARGE_TIME)
#EssentTest2013$ch3 <- round(EssentTest2013$ch, digits = 0) Hij rond af naar beneden. Ook bij 00:59
EssentTest2013$ch2 <- 60 * hours(ch) + minutes(ch)
EssentTest2013$time_min <- (EssentTest2013$kWh4/EssentTest2013$ch2)

str(EssentTest2013$IS_SUM_RECORD)
EssentTest2013$IS_SUM_RECORD("X")

str(NuonJanClean$CONNECT_TIME)
NuonJanClean$CONNECT_TIME <- as.character(NuonJanClean$CONNECT_TIME)
NuonJanClean$tmp_00 <- ":00"
NuonJanClean$CONNECT_MIN <- with(NuonJanClean, paste0(CONNECT_TIME, tmp_00))
View(NuonJanClean$CONNECT_MIN)
NuonJanClean$CONNECT_MIN <- gsub("h", ":", NuonJanClean$CONNECT_MIN, fixed = TRUE)
NuonJanClean$CONNECT_MIN <- gsub("min", "", NuonJanClean$CONNECT_MIN, fixed = TRUE)
NuonJanClean$CONNECT_MIN <- times(NuonJanClean$CONNECT_MIN) # Niet de juiste methode! Houdt zich aan dagen van 24 uur. Langer dan 24 uur connectie = NA
NuonJanClean$CONNECT_MIN <- 60*hours(ch) + minutes(ch)

View(NuonJanClean)

ST_DF <- function (obj){
  obj$Address <- paste(obj$Street, obj$HouseNumber, sep="_")
  CP_obj <- SpatialPoints(obj[,c("Longitude","Latitude")])
  proj4string(CP_obj) <- CRS("+proj=longlat +datum=WGS84")
  obj$BEGIN_CS <- as.POSIXct(paste(obj$BEGIN_CS), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  obj$END_CS <- as.POSIXct(paste(obj$END_CS), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  CP_obj.st <- STIDF(CP_obj, time=obj$BEGIN_CS, data=obj[,c("Address", "Provider","kWh", "Weekday")], endTime=obj$END_CS)
  return (CP_obj.st)
} 

NuonSTIDF <- ST_DF(NuonTest)
View(NuonTest)

require(plotKML)
require(spacetime)
require(plyr)
require(RSAGA)
require(rgdal)
require(raster)
require(sp)

Session_vertical <- function (obj, kml.name){ 
  obj.sp <- obj
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  kml_open(kml.name)
  kml_legend.bar(obj.sp$Weekday, legend.pal=SAGA_pal[[1]][c(1,7)], legend.file = "Weekday.png")
  kml_screen(image.file = "Weekday.png", position = "UL", sname = "Weekday")
  kml_layer.SpatialPoints(obj.sp[c("kWh","Address", "Provider", "BEGIN_CS", "END_CS", "Weekday")], TimeSpan.begin=format(obj.sp$BEGIN_CS, "%Y-%m-%dT%H:%M:%SZ"), TimeSpan.end=format(obj.sp$END_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh*10, colour=obj.sp$Weekday, colour_scale=SAGA_pal[[1]][c(1,7)], shape=shape, labels="", altitudeMode="relativeToGround", balloon = TRUE, legend = TRUE)
  kml_close(kml.name)
  kml_View(kml.name)
} 

Session_vertical(NuonTest, "NuonSTIDFweekday.kml")

View(NuonTest)

obj.sp <- NuonSTIDF
str(obj.sp)
coordinates(obj.sp) <- ~Longitude + Latitude
proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
kml_open("NuonSTIDFweekday.kml")
kml_layer.SpatialPoints(obj.sp[c("kWh","Address", "Provider", "BEGIN_CS", "END_CS")], TimeSpan.begin=format(obj$BEGIN_CS, "%Y-%m-%dT%H:%M:%SZ"), TimeSpan.end=format(obj$END_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh*10, colour=Weekday, colour_scale=R_pal[["heat_colors"]], shape=shape, labels="", altitudeMode="relativeToGround", balloon = TRUE)
kml_close("NuonSTIDFweekday.kml")
kml_View("NuonSTIDFweekday.kml")

shape <- "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
kml(NuonSTIDF[1:5000], colour=log1p(kWh), shape=shape, labels="", kmz=TRUE)
kml_View("NuonSTIDF.kmz")

plotKML(NuonSTIDF)



library(sp)
coordinates(NuonTest) <- ~Longitude + Latitude



NuonTest <- NuonJanClean

NuonTest$Address <- paste(NuonTest$Street,NuonTest$HouseNumber, sep="_")
CP_NUON01 <- SpatialPoints(NuonTest[,c("Longitude","Latitude")])
proj4string(CP_NUON01) <- CRS("+proj=longlat +datum=WGS84")
hist(NuonTest$kWh)
str(NuonTest)
View(NuonTest)
NuonTest$BEGIN_CS <- as.POSIXct(paste(NuonTest$BEGIN_CS), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
NuonTest$END_CS <- as.POSIXct(paste(NuonTest$END_CS), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
CP_NUON01.st <- STIDF(CP_NUON01, time=NuonTest$BEGIN_CS, data=NuonTest[,c("Address", "Provider","kWh", "Weekday")], endTime=NuonTest$END_CS)
View(CP_NUON01.st)

str(ch)
plot(ch2)
plot(EssentTest2013$CHARGETIME)
