
library(plotKML)
library(spacetime)
library(plyr)
library(RColorBrewer)

setwd("H:/Daniel")
dat <- read.csv("rapportage_verbruiksdata.csv")
str(dat)
dat$ID <- paste(dat$Straat, dat$Huisnummer, dat$Postcode, sep="_")
xy <- read.csv("ChargingStations.csv", sep = ";")
str(xy)
xy$ID <- paste(xy$Street, xy$HouseNumber, xy$PostalCode, sep="_")
dat.xy <- join(dat, xy, by="ID")
str(dat.xy)
dat.xy$Tbegin <- as.POSIXct(paste(dat.xy$Start), format="%d-%m-%Y %H:%M")
dat.xy$Tend <- as.POSIXct(paste(dat.xy$Eind), format="%d-%m-%Y %H:%M")
summary(dat.xy$Tend)
summary(dat.xy$Tbegin)
dat.xy <- dat.xy[!is.na(dat.xy$Latitude),]

# create a STIDF object:
sp <- SpatialPoints(dat.xy[,c("Longitude","Latitude")])
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
hist(dat.xy$kWh) ## log-normal
dat.st <- STIDF(sp, time=dat.xy$Tbegin, data=dat.xy[,c("ID","kWh")], endTime=dat.xy$Tend)
#shape <- "http://maps.google.com/mapfiles/kml/paddle/wht-blank.png"
shape <- "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
kml(dat.st[1:5000], colour=log1p(kWh), shape=shape, labels="", kmz=TRUE)
kml_View("dat.st.kmz")
#stplot(dat.st[1:100])

## alternative:
dat.sp <- dat.xy
coordinates(dat.sp) <- ~ Longitude + Latitude
proj4string(dat.sp) <- CRS("+proj=longlat +datum=WGS84")
kml_open("sp_test.kml")
kml_layer.SpatialPoints(dat.sp, TimeSpan.begin=format(dat.sp$Tbegin, "%Y-%m-%dT%H:%M:%SZ"), TimeSpan.end=format(dat.sp$Tend, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh*10, colour=log1p(kWh), shape=shape, labels="", altitudeMode="relativeToGround")
kml_close("sp_test.kml")
kml_View("sp_test.kml")

library(plotGoogleMaps)
summary(dat.st$kWh)
m <- stplotGoogleMaps(dat.st, zcol="kWh", colPalette=brewer.pal(9, "YlOrRd"), do.bubble=TRUE, fillOpacity=0.85, mapTypeId='ROADMAP', w='49%', h='49%')
