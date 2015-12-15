# Split (subset) Nuon files
EssentTest <- read.csv("exp_201301-62014.csv", header = T, sep=",")
View(EssentTest)
EssentTest$BEGIN_LOAD_DATE <- as.POSIXct(paste(EssentTest$BEGIN_LOAD_DATE), format="%d.%m.%Y", tz = "GMT")
EssentTest2013 <- subset(EssentTest, BEGIN_LOAD_DATE < as.POSIXct("2013-01-05 00:00"))
EssentTest2013 <- subset(EssentTest2013, BEGIN_LOAD_DATE > as.POSIXct("2013-01-01 00:00"))
View(EssentTest2013)
write.csv(EssentTest2013, file= paste("EssentTest2013", "csv", sep = "."))

if (!require(plyr)) install.packages('plyr')
EssentTest2013$kWh <- as.numeric(EssentTest2013$ENERGIE)
EssentTest2013$kWh2 <- as.character(EssentTest2013$ENERGIE)
EssentTest2013$kWh3 <- as.numeric(EssentTest2013$kWh2)

EssentTest2013$kWh2 <- gsub(",", "", EssentTest2013$kWh2, fixed = TRUE)
EssentTest2013$kWh3 <- as.numeric(EssentTest2013$kWh2)
EssentTest2013$kWh4 <- (EssentTest2013$kWh3/10000)
