# Purpose        : Preprocess Charge point and Charge session data for use for ESDA tool;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Last update    : 04-12-2015
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
if (!require(plyr)) install.packages('plyr')
if (!require(RCurl)) install.packages('RCurl')

#-------------------------------------------------------------------------------------------  
# pre-process Charge Point Dataset (latitude/longitude)
#-------------------------------------------------------------------------------------------
get.stations <- function(webaddress, file.name){
  # Download Charge point dataset
  download.file(webaddress,destfile=file.name,method="libcurl")
  Stations <- read.csv(file.name, header = T, sep=";")
  # Remove white space from PostalCode
  Stations$PostalCode <- gsub(" ", "", Stations$PostalCode, fixed = T)
  # Create address for join opperation
  Stations$Address <- paste(Stations$Street, Stations$HouseNumber, Stations$PostalCode, sep=" ")
  # Remove double entries based on unique values in column "CPExternalID" 
  Stations <- Stations[ !duplicated(Stations["CPExternalID"]),]
} 

# get.stations("https://api.essent.nl/generic/downloadChargingStations?latitude_low=52.30567123031878&longtitude_low=4.756801078125022&latitude_high=52.43772606594848&longtitude_high=5.086390921875022&format=CSV", "ChargeStations.csv")
#-------------------------------------------------------------------------------------------  
# pre-process Nuon charge session dataset
#-------------------------------------------------------------------------------------------

# Mannualy put charge data into workspace directory and save as CSV-file!!
list.files()
csv.file <- "rapportage_verbruiksdata 201301 + 201306.csv"
prep_NUON <- function (csv.file){
  # Read csv files and create R-objects
  NuonRaw <- read.csv(csv.file,  header = T, sep=",")
  # Remove double sessions  
  NuonRaw <- NuonRaw[ !duplicated(NuonRaw["Sessie"]),] # Why are there double sessions in the first place?
  # Set date and time 
  NuonRaw$BEGIN_CS <- as.POSIXct(paste(NuonRaw$Start), format="%d-%m-%Y %H:%M", tz = "GMT")
  NuonRaw$END_CS <- as.POSIXct(paste(NuonRaw$Eind), format="%d-%m-%Y %H:%M",  tz = "GMT")
    # Rename columns: 
  names(NuonRaw)[names(NuonRaw)=="Straat"] <- "Street"
  names(NuonRaw)[names(NuonRaw)=="Huisnummer"] <- "HouseNumber"
  names(NuonRaw)[names(NuonRaw)=="Postcode"] <- "PostalCode"
  names(NuonRaw)[names(NuonRaw)=="Laadtijd"] <- "CONNECT_TIME"
  names(NuonRaw)[names(NuonRaw)=="Sessie"] <- "Session_ID"
  # Remove white space from PostalCode
  NuonRaw$PostalCode <- gsub(" ", "", NuonRaw$PostalCode, fixed = T)
  # Join Charge data with xy-coordinates
  NuonRaw$Address <- paste(NuonRaw$Street, NuonRaw$HouseNumber, NuonRaw$PostalCode, sep=" ")
  View(NuonRaw)
  NuonRaw.Stations <- join(NuonRaw, Stations, by="Address", type = "left")
  # Remove duplicates in joined file 
  NuonRaw.Sessions <- NuonRaw.Stations[ !duplicated(NuonRaw.Stations["Session_ID"]),]
  # Remove NA values in Latitude column 
  NuonRaw.Sessions <- NuonRaw.Sessions[!is.na(NuonRaw.Sessions$Latitude),] # Many failed matches (2778!) 
  #Maybe because of case sensitive join opperation?
  View(EssentRaw01)
  # Remove unnecessary columns
  keep <- c("Session_ID", "BEGIN_CS", "END_CS", "CONNECT_TIME", "kWh", "Street", "HouseNumber", "PostalCode", "Address", "Latitude", "Longitude", "Provider")
  NuonRaw_clean <- NuonRaw.Sessions[keep]
  # Split Nuon dataset into Januari and June 
  NuonClean01 <- subset(NuonRaw_clean, BEGIN_CS <= as.POSIXct("2013-01-31 00:00"))
  NuonClean06 <- subset(NuonRaw_clean, BEGIN_CS > as.POSIXct("2013-01-31 00:00"))
} 
#-------------------------------------------------------------------------------------------  
# pre-process Essent charge session dataset
#-------------------------------------------------------------------------------------------
# Mannualy put charge data into workspace directory and save as CSV-file!!
list.files()

# Read csv files and create R-objects
EssentRaw01 <- read.csv("exp_201301-62014.csv",  header = T, sep=",")
EssentRaw06 <- read.csv("exp_201306-62014.csv",  header = T, sep=",")

# Set date and time 
EssentRaw01$BEGIN_DA <- as.character(EssentRaw01$BEGIN_LOAD_DATE)
EssentRaw01$BEGIN_TI <- as.character(EssentRaw01$BEGIN_LOAD_TIME)
EssentRaw01$END_DA <- as.character(EssentRaw01$END_LOAD_DATE)
EssentRaw01$END_TI <- as.character(EssentRaw01$END_LOAD_TIME)
EssentRaw01$BEGIN_CS <- as.POSIXct(paste(EssentRaw01$BEGIN_DA, EssentRaw01$BEGIN_TI), format="%d.%m.%Y %H:%M:%S", tz = "GMT")
EssentRaw01$END_CS <- as.POSIXct(paste(EssentRaw01$END_DA, EssentRaw01$END_TI), format="%d.%m.%Y %H:%M:%S",  tz = "GMT")

EssentRaw06$BEGIN_DA <- as.character(EssentRaw06$BEGIN_LOAD_DATE)
EssentRaw06$BEGIN_TI <- as.character(EssentRaw06$BEGIN_LOAD_TIME)
EssentRaw06$END_DA <- as.character(EssentRaw06$END_LOAD_DATE)
EssentRaw06$END_TI <- as.character(EssentRaw06$END_LOAD_TIME)
EssentRaw06$BEGIN_CS <- as.POSIXct(paste(EssentRaw06$BEGIN_DA, EssentRaw06$BEGIN_TI), format="%d.%m.%Y %H:%M:%S", tz = "GMT")
EssentRaw06$END_CS <- as.POSIXct(paste(EssentRaw06$END_DA, EssentRaw06$END_TI), format="%d.%m.%Y %H:%M:%S",  tz = "GMT")
# Convert energy from factor to numeric
EssentRaw01$ENERGIE <- as.numeric(EssentRaw01$ENERGIE)
EssentRaw06$ENERGIE <- as.numeric(EssentRaw06$ENERGIE)

# Rename columns: 
names(EssentRaw01)[names(EssentRaw01)=="STREET"] <- "Street"
names(EssentRaw01)[names(EssentRaw01)=="HOUSE_NUM1"] <- "HouseNumber"
names(EssentRaw01)[names(EssentRaw01)=="POST_CODE1"] <- "PostalCode"
names(EssentRaw01)[names(EssentRaw01)=="CHARGE_DURATION"] <- "CONNECT_TIME"
names(EssentRaw01)[names(EssentRaw01)=="ENERGIE"] <- "kWh"
names(EssentRaw01)[names(EssentRaw01)=="UNIQUE_ID"] <- "Session_ID"

names(EssentRaw06)[names(EssentRaw06)=="STREET"] <- "Street"
names(EssentRaw06)[names(EssentRaw06)=="HOUSE_NUM1"] <- "HouseNumber"
names(EssentRaw06)[names(EssentRaw06)=="POST_CODE1"] <- "PostalCode"
names(EssentRaw06)[names(EssentRaw06)=="CHARGE_DURATION"] <- "CONNECT_TIME"
names(EssentRaw06)[names(EssentRaw06)=="ENERGIE"] <- "kWh"
names(EssentRaw06)[names(EssentRaw06)=="UNIQUE_ID"] <- "Session_ID"

# Remove white space from PostalCode
EssentRaw01$PostalCode <- as.character(EssentRaw01$PostalCode)
EssentRaw01$PostalCode <- gsub(" ", "", EssentRaw01$PostalCode, fixed = T)
EssentRaw06$PostalCode <- as.character(EssentRaw06$PostalCode)
EssentRaw06$PostalCode <- gsub(" ", "", EssentRaw06$PostalCode, fixed = T)

# Join Charge data with xy-coordinates
EssentRaw01$Address <- paste(EssentRaw01$Street, EssentRaw01$HouseNumber, EssentRaw01$PostalCode, sep=" ")
EssentRaw01.Stations <- join(EssentRaw01, Stations, by="Address", type = "left", match = "all")
EssentRaw06$Address <- paste(EssentRaw06$Street, EssentRaw06$HouseNumber, EssentRaw06$PostalCode, sep=" ")
EssentRaw06.Stations <- join(EssentRaw06, Stations, by="Address", type = "left")
View(EssentRaw06.Stations)
# Remove NA values in Latitude column 
EssentRaw01.Stations <- EssentRaw01.Stations[!is.na(EssentRaw01.Stations$Latitude),] 
EssentRaw06.Stations <- EssentRaw06.Stations[!is.na(EssentRaw06.Stations$Latitude),] 
View(EssentRaw01.Stations)
str(keep)
# Remove unnecessary columns
EssentClean01 <- EssentRaw01.Stations[keep]
EssentClean06 <- EssentRaw06.Stations[keep]

View(EssentClean01)
View(EssentClean06)

#-------------------------------------------------------------------------------------------  
# Write object to csv file for viewing outside R environment
#-------------------------------------------------------------------------------------------  
write.csv(NuonClean01, file= "NuonClean01.csv")
write.csv(NuonClean06, file= "NuonClean06.csv")
write.csv(EssentClean01, file = "EssentClean01.csv")
write.csv(EssentClean06, file = "EssentClean06.csv")
write.csv(Stations, file = "StationsClean.csv")