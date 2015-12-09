# Purpose        : Main script for creating an ESDA raster visualization in Google Earth from Charge point data;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Note           : 

# Set working directory


# Download and open required packages
if (!require(plyr)) install.packages('plyr')
if (!require(RCurl)) install.packages('RCurl')

# Aquire charge stations
get.stations("https://api.essent.nl/generic/downloadChargingStations?latitude_low=52.30567123031878&longtitude_low=4.756801078125022&latitude_high=52.43772606594848&longtitude_high=5.086390921875022&format=CSV", "ChargeStations.csv")
str(Stations)
# Make kml file out of stations, for png inspiration: http://kml4earth.appspot.com/icons.html
kml_stations("ChargeStations.csv", "http://maps.google.com/mapfiles/kml/paddle/wht-blank.png", "Stations2015.kml", legend=TRUE, balloon=TRUE)

# Mannualy put charge data into workspace directory and save as CSV-file!!
list.files()
# PreProcess Nuon dataset
prep_NUON("rapportage_verbruiksdata 201301 + 201306.csv")

# PreProcess Essent dataset

