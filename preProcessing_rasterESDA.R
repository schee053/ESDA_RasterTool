# Purpose        : Preprocess Charge point and Charge session data for use for ESDA tool;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Last update    : 13-12-2015
# Note           : Subscript of main_rasterESDA.R


# Set directory
getwd()

# Download and open required packages
if (!require(plyr)) install.packages('plyr')
if (!require(RCurl)) install.packages('RCurl')
if (!require(chron)) install.packages('chron')

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
  # Write to csv 
  write.csv(Stations, file= paste(file.name, "csv", sep = "."))
  return (Stations)
} 

Stations <- get.stations("https://api.essent.nl/generic/downloadChargingStations?latitude_low=52.30567123031878&longtitude_low=4.756801078125022&latitude_high=52.43772606594848&longtitude_high=5.086390921875022&format=CSV", "ChargeStations")
#-------------------------------------------------------------------------------------------  
# pre-process Nuon charge session dataset
#-------------------------------------------------------------------------------------------

# Mannualy put charge data into workspace directory and save as CSV-file!!
list.files()

# Split (subset) Nuon files
NuonSplit <- read.csv("rapportage_verbruiksdata 201301 + 201306.csv", header = T, sep=",")
NuonSplit$BEGIN_CS <- as.POSIXct(paste(NuonSplit$Start), format="%d-%m-%Y %H:%M", tz = "GMT")
NuonSplit$END_CS <- as.POSIXct(paste(NuonSplit$Eind), format="%d-%m-%Y %H:%M",  tz = "GMT")
NuonJanuari2013 <- subset(NuonSplit, BEGIN_CS <= as.POSIXct("2013-01-31 00:00"))
NuonJune2013 <- subset(NuonSplit, BEGIN_CS > as.POSIXct("2013-01-31 00:00"))
write.csv(NuonJanuari2013, file= paste("NuonJanuari2013", "csv", sep = "."))
write.csv(NuonJune2013, file= paste("NuonJune2013", "csv", sep = "."))

prep_NUON <- function (csv.file, obj.name){
  # Read csv files and create R-objects
  NuonRaw <- read.csv(csv.file,  header = T, sep=",")
  # Remove double sessions  
  NuonRaw <- NuonRaw[ !duplicated(NuonRaw["Sessie"]),] # Why are there double sessions in the first place?
  # Set date and time 
    # NuonRaw$BEGIN_CS <- as.POSIXct(paste(NuonRaw$Start), format="%d-%m-%Y %H:%M", tz = "GMT")
    # NuonRaw$END_CS <- as.POSIXct(paste(NuonRaw$Eind), format="%d-%m-%Y %H:%M",  tz = "GMT")
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
  NuonRaw.Stations <- join(NuonRaw, Stations, by="Address", type = "left")
  # Remove duplicates in joined file 
  NuonRaw.Sessions <- NuonRaw.Stations[ !duplicated(NuonRaw.Stations["Session_ID"]),]
  # Remove NA values in Latitude column 
  NuonRaw.Sessions <- NuonRaw.Sessions[!is.na(NuonRaw.Sessions$Latitude),] # Many failed matches (2778!) 
  # Add weekdays column
  NuonRaw.Sessions$Weekday <- weekdays(as.Date(NuonRaw.Sessions$BEGIN_CS, "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  #Maybe because of case sensitive join opperation?
  #View(NuonRaw)
  # Remove unnecessary columns
  keep <- c("Session_ID", "BEGIN_CS", "END_CS", "Weekday", "CONNECT_TIME", "kWh", "Street", "HouseNumber", "PostalCode", "Address", "Latitude", "Longitude", "Provider")
  NuonClean <- NuonRaw.Sessions[keep]
  # Write to csv and return object
  write.csv(NuonClean, file= paste(obj.name, "csv", sep = "."))
  return(NuonClean)
} 

# Run function
Nuon_Januari2013 <- prep_NUON("NuonJanuari2013.csv", "Nuon_Januari2013")
Nuon_June2013 <- prep_NUON("NuonJune2013.csv", "Nuon_June2013") 

#-------------------------------------------------------------------------------------------  
# pre-process Essent charge session dataset
#-------------------------------------------------------------------------------------------
# Mannualy put charge data into workspace directory and save as CSV-file!!
list.files()

prep_ESSENT <- function(csv.file, obj.name){
  # Read CSV file
  EssentRaw <- read.csv(csv.file,  header = T, sep=",")
  # Set date and time 
  EssentRaw$BEGIN_DA <- as.character(EssentRaw$BEGIN_LOAD_DATE)
  EssentRaw$BEGIN_TI <- as.character(EssentRaw$BEGIN_LOAD_TIME)
  EssentRaw$END_DA <- as.character(EssentRaw$END_LOAD_DATE)
  EssentRaw$END_TI <- as.character(EssentRaw$END_LOAD_TIME)
  EssentRaw$BEGIN_CS <- as.POSIXct(paste(EssentRaw$BEGIN_DA, EssentRaw$BEGIN_TI), format="%d.%m.%Y %H:%M:%S", tz = "GMT")
  EssentRaw$END_CS <- as.POSIXct(paste(EssentRaw$END_DA, EssentRaw$END_TI), format="%d.%m.%Y %H:%M:%S",  tz = "GMT")
  
  # Remove sessions from December 2012
  EssentRaw <- subset(EssentRaw, BEGIN_CS > as.POSIXct("2013-01-01 00:00"))
  
  # Convert energy from factor to numeric
  EssentRaw$ENERGIE <- as.character(EssentRaw$ENERGIE)
  EssentRaw$ENERGIE <- gsub(",", "", EssentRaw$ENERGIE, fixed = TRUE)
  EssentRaw$ENERGIE <- as.numeric(EssentRaw$ENERGIE)
  EssentRaw$ENERGIE <- (EssentRaw$ENERGIE / 10000)
  
  # Rename columns: 
  names(EssentRaw)[names(EssentRaw)=="STREET"] <- "Street"
  names(EssentRaw)[names(EssentRaw)=="HOUSE_NUM1"] <- "HouseNumber"
  names(EssentRaw)[names(EssentRaw)=="POST_CODE1"] <- "PostalCode"
  names(EssentRaw)[names(EssentRaw)=="CHARGE_DURATION"] <- "CONNECT_TIME"
  names(EssentRaw)[names(EssentRaw)=="ENERGIE"] <- "kWh"
  names(EssentRaw)[names(EssentRaw)=="UNIQUE_ID"] <- "Session_ID"
  
  # Remove white space from PostalCode
  EssentRaw$PostalCode <- as.character(EssentRaw$PostalCode)
  EssentRaw$PostalCode <- gsub(" ", "", EssentRaw$PostalCode, fixed = T)
  
  # Join Charge data with xy-coordinates
  EssentRaw$Address <- paste(EssentRaw$Street, EssentRaw$HouseNumber, EssentRaw$PostalCode, sep=" ")
  EssentRaw.Stations <- join(EssentRaw, Stations, by="Address", type = "left", match = "all")

  # Remove duplicates in joined file 
  EssentRaw.Stations$REMOVE_ID <- paste(EssentRaw.Stations$Session_ID, EssentRaw.Stations$METER_READ_BEGIN, EssentRaw.Stations$Address)
  EssentRaw.Sessions <- EssentRaw.Stations[ !duplicated(EssentRaw.Stations["REMOVE_ID"]),]
  # Not the right combination of joins! --> find out where the duplicates come from! 
  
  # Remove NA values in Latitude column 
  EssentRaw.Sessions <- EssentRaw.Sessions[!is.na(EssentRaw.Sessions$Latitude),] 
  
  # Add weekdays column
  EssentRaw.Sessions$Weekday <- weekdays(as.Date(EssentRaw.Sessions$BEGIN_CS, "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  
  # Remove unnecessary columns
  keep <- c("Session_ID", "BEGIN_CS", "END_CS", "Weekday", "CONNECT_TIME", "kWh", "Street", "HouseNumber", "PostalCode", "Address", "Latitude", "Longitude", "Provider")
  EssentClean <- EssentRaw.Sessions[keep]
  
  # Write to csv and return object
  write.csv(EssentClean, file = paste(obj.name, "csv", sep =".")) 
  return (EssentClean)
}

# Run function
Essent_Januari2013 <- prep_ESSENT("exp_201301-62014.csv", "Essent_Januari2013")
Essent_June2013 <- prep_ESSENT("exp_201306-62014.csv", "Essent_June2013")
