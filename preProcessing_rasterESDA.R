# Purpose        : Preprocess Charge point and Charge session data for use for ESDA tool;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Note           : Subscript of main_rasterESDA.R


# Set directory
mainDir <- "M:/GeoDataMscThesis/"
subDir <- "Datasets"
outputDir <- 
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

# Download and open required packages
require(plyr)

#-------------------------------------------------------------------------------------------  
# pre-process Charge Point Dataset (latitude/longitude)
#-------------------------------------------------------------------------------------------

# Download Charge point dataset
download.file("https://api.essent.nl/generic/downloadChargingStations?latitude_low=52.30567123031878&longtitude_low=4.756801078125022&latitude_high=52.43772606594848&longtitude_high=5.086390921875022&format=CSV",destfile="ChargeStations.csv",method="libcurl")
ChargeStations <- read.csv("ChargeStations.csv", header = T, sep=";")
# Remove doubles
# Create unique ID
ChargeStations$UNIQUE_ID <- paste(ChargeStations$Latitude, ChargeStations$Longitude, ChargeStations$CPExternalID, sep = "_")
# Remove double entries based on unique values in 19th column (UNIQUE_ID)
Stations <- ChargeStations[ !duplicated(ChargeStations[ ,19]),]

#-------------------------------------------------------------------------------------------  
# pre-process Nuon charge session dataset
#-------------------------------------------------------------------------------------------
# Mannualy put charge data into Datasets directory and save as CSV-file.
list.files()

# Read csv files and create R-objects
NuonRaw <- read.csv("rapportage_verbruiksdata 201301 + 201306.csv",  header = T, sep=",")
# Set date and time (Nuon)
NuonRaw$BEGIN_CS <- as.POSIXct(paste(NuonRaw$Start), format="%d-%m-%Y %H:%M", tz = "GMT")
NuonRaw$END_CS <- as.POSIXct(paste(NuonRaw$Eind), format="%d-%m-%Y %H:%M",  tz = "GMT")
# Split Nuon dataset into Januari and June 
NuonRaw01 <- subset(NuonRaw, BEGIN_CS <= as.POSIXct("2013-01-31 00:00"))
NuonRaw06 <- subset(NuonRaw, BEGIN_CS > as.POSIXct("2013-01-31 00:00"))
# Rename column by name: (for merge purposes)
names(NuonRaw01)[names(NuonRaw01)=="Straat"] <- "Street"
names(NuonRaw01)[names(NuonRaw01)=="Huisnummer"] <- "HouseNumber"
names(NuonRaw01)[names(NuonRaw01)=="Postcode"] <- "PostalCode"
names(NuonRaw01)[names(NuonRaw01)=="Laadtijd"] <- "CONNECT_TIME"

#-------------------------------------------------------------------------------------------  
# pre-process Essent charge session dataset
#-------------------------------------------------------------------------------------------
# Read csv files and create R-objects
EssentRaw01 <- read.csv("exp_201301-62014.csv",  header = T, sep=",")
EssentRaw06 <- read.csv("exp_201306-62014.csv",  header = T, sep=",")

# Rename column by name: (for merge purposes)
names(EssentRaw01)[names(EssentRaw01)=="STREET"] <- "Street"
names(EssentRaw01)[names(EssentRaw01)=="HOUSE_NUM1"] <- "HouseNumber"
names(EssentRaw01)[names(EssentRaw01)=="POST_CODE1"] <- "PostalCode"
names(EssentRaw01)[names(EssentRaw01)=="CHARGE_DURATION"] <- "CONNECT_TIME"
names(EssentRaw01)[names(EssentRaw01)=="ENERGIE"] <- "kWh"

# Merge date and time columns (Essent)
as.POSIXct(EssentRaw01$BEGIN_LOAD_DATE, format = "%d.%m.%Y")
as.POSIXct(EssentRaw01$BEGIN_LOAD_TIME, format = "%H:%M:%S", tz = "GMT")
as.POSIXct(EssentRaw01$END_LOAD_DATE, format = "%d.%m.%Y")
as.POSIXct(EssentRaw01$END_LOAD_TIME, format = "%H:%M:%S", tz = "GMT")
EssentRaw01$BEGIN_CS <- as.POSIXct(paste(EssentRaw01$BEGIN_LOAD_DATE, EssentRaw01$BEGIN_LOAD_TIME), format = "%Y-%m-%d %H:%M:%S")
EssentRaw01$END_CS <- as.POSIXct(paste(EssentRaw01$END_LOAD_DATE, EssentRaw01$END_LOAD_TIME), format = "%Y-%m-%d %H:%M:%S")

EssentRaw01$BEGIN_CS <- cbind(EssentRaw01$BEGIN_LOAD_DATE, EssentRaw01$BEGIN_LOAD_TIME) # Gives weird numbers

#-------------------------------------------------------------------------------------------  
# Merge charge data with charge point coordinates
#-------------------------------------------------------------------------------------------  
mergeXY <- function (df, xy){
  # (df, enter charge data)
  df$ID <- paste(df$Street, df$HouseNumber, df$PostalCode, sep="_")
  # (xy, enter charge points)
  xy$ID <- paste(xy$Street, xy$HouseNumber, xy$PostalCode, sep="_")
  df.xy <- join(df, xy, by="ID")
  return (df.xy)
}

# Remove na values
dat.xy <- dat.xy[!is.na(dat.xy$Latitude),]

# Remove unnecessary columns
keep <- c("Street", "HouseNumber", "PostalCode", "BEGIN_CS", "END_CS", "CONNECT_TIME", "kWh", "Latitude", "Longitude", "Provider")

Clean <- function (df){
  return (df[keep])
}
#EssentClean01 <- EssentRaw01[keep]
#NuonClean01 <- NuonClean01[keep]

#-------------------------------------------------------------------------------------------  
# Write object to csv file for viewing outside R environment
#-------------------------------------------------------------------------------------------  

WriteCSV <- function (df){
  write.csv(df, file = paste(df + ".csv")) # ERROR, df + ".csv" : non-numeric argument to binary operator 
}

#write.csv(EssentClean01, file = "EssentClean01.csv")
#write.csv(NuonClean01, file= "NuonClean01.csv")