# Purpose        : Transform shapefiles to KML;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Last update    : 13-12-2015
# Note           : Subscript of main_rasterESDA.R

# Set directory
mainDir <- "M:/MyDocuments/ESDA_RasterTool/"
subDir <- "Datasets"
outputDir <- "Output"
dir.create(file.path(mainDir, subDir), showWarnings = F)
dir.create(file.path(mainDir, outputDir), showWarnings = F)
setwd(file.path(mainDir, subDir))
getwd()

# Download and open required packages
if (!require(raster)) install.packages('raster')
if (!require(plotKML)) install.packages('plotKML')
if (!require(spatstat)) install.packages('spatstat')
if (!require(maptools)) install.packages('maptools')
if (!require(RCurl)) install.packages('RCurl')
if (!require(rgdal)) install.packages('rgdal')

# http://maps.amsterdam.nl/open_geodata/
download.file("http://maps.amsterdam.nl/gebiedsindeling/GEBIEDSINDELINGEN.ZIP",destfile= paste("Areas", "zip", sep = "."),method="libcurl")

# Manually extract required files
# Set projection:
P4S.latlon <- CRS("+init=epsg:3857")
tmp_stadsdelen <- readShapePoly("M:/My Documents/ESDA_RasterTool/Stadsdelen_region.shp", verbose = FALSE, proj4string = P4S.latlon)

Stadsdelen <- spTransform(tmp_stadsdelen, CRS("+proj=longlat +datum=WGS84"))
plotKML(Stadsdelen) # Ends up in the sea
plot(Stadsdelen, axes=TRUE, border="grey")

?readShapePoly
