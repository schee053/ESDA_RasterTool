# Purpose        : Create rasterbrick spacetime object from charge session dataset;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Note           : Subscript of main_rasterESDA.R


vals <- c(10, 29 , 90, 20 ,1)

for( st in 1:nrow(Stations)){
  
  b1 <- brick(nrows=1, ncols=1,nl=length(vals),
              xmn=Stations$Longitude[st]-0.001, xmx=Stations$Longitude[st]+0.001, ymn=Stations$Latitude[st]-0.001, ymx=Stations$Latitude[st]+0.001)
  b1[1] <- vals
  b1
  
}



rasterize()

sp_stations <- Stations
coordinates(sp_stations) <- ~Longitude +Latitude
sp_stations$house_number <- as.numeric(sp_stations$HouseNumber)

b1 <- raster(extent(sp_stations), res=0.001)
b2 <- rasterize(sp_stations, b1, field="house_number")
plot(b2)
