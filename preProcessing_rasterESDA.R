# Purpose        : Preprocess Charge point and Charge session data for use for ESDA tool;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Last update    : 04-12-2015
# Note           : Subscript of main_rasterESDA.R


# Set directory
mainDir <- "E:/ESDA_RasterTool/"
subDir <- "Datasets"
outputDir <- "Output"
dir.create(file.path(mainDir, subDir), showWarnings = F)
dir.create(file.path(mainDir, outputDir), showWarnings = F)
setwd(file.path(mainDir, subDir))

# Download and open required packages
if (!require(plyr)) install.packages('plyr')
if (!require(RCurl)) install.packages('RCurl')
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
# Remove double sessions  
NuonRaw <- NuonRaw[ !duplicated(NuonRaw[ ,14]),] # Why are there double sessions in the first place?
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
Stations$PostalCode <- gsub(" ", "", Stations$PostalCode, fixed = T)
# Join Charge data with xy-coordinates
NuonRaw$join_ID <- paste(NuonRaw$Street, NuonRaw$HouseNumber, NuonRaw$PostalCode, sep="_")
Stations$join_ID <- paste(Stations$Street, Stations$HouseNumber, Stations$PostalCode, sep="_")
NuonRaw.Stations <- join(NuonRaw, Stations, by="join_ID", type = "left")
# Remove duplicates in joined file 
NuonRaw.Sessions <- NuonRaw.Stations[ !duplicated(NuonRaw.Stations[ ,14]),]
# Remove NA values in Latitude column 
NuonRaw.Sessions <- NuonRaw.Sessions[!is.na(NuonRaw.Sessions$Latitude),] # Many failed matches (2778!)
# Remove unnecessary columns
keep <- c("Session_ID", "BEGIN_CS", "END_CS", "CONNECT_TIME", "kWh", "Street", "HouseNumber", "PostalCode", "Latitude", "Longitude", "Provider")
NuonRaw_clean <- NuonRaw.Sessions[keep]
# Split Nuon dataset into Januari and June 
NuonRaw01 <- subset(NuonRaw_clean, BEGIN_CS <= as.POSIXct("2013-01-31 00:00"))
NuonRaw06 <- subset(NuonRaw_clean, BEGIN_CS > as.POSIXct("2013-01-31 00:00"))

View(NuonRaw01)
#-------------------------------------------------------------------------------------------  
# pre-process Essent charge session dataset
#-------------------------------------------------------------------------------------------
# Read csv files and create R-objects
EssentRaw01 <- read.csv("exp_201301-62014.csv",  header = T, sep=",")
EssentRaw06 <- read.csv("exp_201306-62014.csv",  header = T, sep=",")

View(EssentRaw01)

# Set date and time 
EssentRaw01$BEGIN_DA <- as.character(EssentRaw01$BEGIN_LOAD_DATE)
EssentRaw01$BEGIN_TI <- as.character(EssentRaw01$BEGIN_LOAD_TIME)
EssentRaw01$END_DA <- as.character(EssentRaw01$END_LOAD_DATE)
EssentRaw01$END_TI <- as.character(EssentRaw01$END_LOAD_TIME)
EssentRaw01$BEGINCS <- as.POSIXct(paste(EssentRaw01$BEGIN_DA, EssentRaw01$BEGIN_TI), format="%d.%m.%Y %H:%M:%S", tz = "GMT")
EssentRaw01$END_CS <- as.POSIXct(paste(EssentRaw01$END_DA, EssentRaw01$END_TI), format="%d.%m.%Y %H:%M:%S",  tz = "GMT")

# Convert energy to numeric
EssentRaw01$kWh <- as.character(EssentRaw01$ENERGIE)
EssentRaw01$kWh <- EssentRaw01[!is.na(EssentRaw01$kWh),]
EssentRaw01$kWhNum <- paste(EssentRaw01$kWh)


# Remove double sessions  
EssentRaw01 <- EssentRaw01[ !duplicated(EssentRaw01[ ,14]),] # Why are there double sessions in the first place?

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
# Remove white space from PostalCode
EssentRaw01$PostalCode <- gsub(" ", "", EssentRaw01$PostalCode, fixed = T)

# Join Charge data with xy-coordinates
EssentRaw01$join_ID <- paste(EssentRaw01$Street, EssentRaw01$HouseNumber, EssentRaw01$PostalCode, sep="_")
EssentRaw01.Stations <- join(EssentRaw01, Stations, by="join_ID", type = "left")
# Remove duplicates in joined file 
EssentRaw01.Sessions <- EssentRaw01.Stations[ !duplicated(EssentRaw01.Stations[ ,14]),]
# Remove NA values in Latitude column 
EssentRaw01.Sessions <- EssentRaw01.Sessions[!is.na(EssentRaw01.Sessions$Latitude),] # Many failed matches (2778!)
# Remove unnecessary columns
EssentRaw01_clean <- EssentRaw01.Sessions[keep]


View(EssentRaw01)

#-------------------------------------------------------------------------------------------  
# Write object to csv file for viewing outside R environment
#-------------------------------------------------------------------------------------------  
write.csv(NuonClean01, file= "NuonClean01.csv")
write.csv(NuonClean06, file= "NuonClean06.csv")
write.csv(EssentClean01, file = "EssentClean01.csv")
write.csv(EssentClean06, file = "EssentClean06.csv")
write.csv(Stations, file = "StationsClean.csv")