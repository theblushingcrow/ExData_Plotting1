# create directory to serve as working directory
dataDirectory <- "./Exploratory_Data_Analysis_Project_1"
if (!file.exists(dataDirectory)) {dir.create(dataDirectory)}
setwd(dataDirectory)

# download and unzip data file
dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
dateDownloaded <- date()
archiveFilePath <- "./dataArchive.zip"
fileName <- "household_power_consumption.txt"
if(!file.exists(fileName)){
  if(!file.exists(archiveFilePath)){
    download.file(dataUrl, archiveFilePath, method = "curl") #method not needed for Microsoft Windows users
  }
  unzip(archiveFilePath)
}

# read file into memory
firstRow <- read.table(fileName, sep=";", header = TRUE, nrow = 1)
numberOfColumns <- ncol(firstRow)
# to refrain from reading the entire file, we search for the relevant dates and columns
dateColumn <- match("Date",names(firstRow))
columnFilter <- rep("NULL", numberOfColumns)
columnFilter[dateColumn] <- NA
dataDates <- read.table(fileName, sep=";", header = TRUE, as.is = TRUE, colClasses = c(NA, rep("NULL", numberOfColumns - 1)))
relevantStart <- which.max(dataDates$Date == "1/2/2007")
relevantEnd <- which.max(dataDates$Date == "3/2/2007")
globalActivePowerColumn <- match("Global_active_power",names(firstRow))
columnFilter <- rep("NULL", numberOfColumns)
columnFilter[globalActivePowerColumn] <- NA
relevantData <- read.table(fileName, sep = ";", header = TRUE, skip = relevantStart-1, nrows = (relevantEnd-relevantStart), colClasses = columnFilter)

# plot data and save to png
png(filename = "./plot1.png", width = 480, height = 480, units = "px")
hist(relevantData[,1], main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
dev.off()