################
# Course project for Exploratory Data Analysis, week1
# plot1.R
#######################################################
#
# When loading the dataset into R, please consider the following:
#   
# - The dataset has 2,075,259 rows and 9 columns. First calculate a rough estimate of how much memory the dataset 
#   will require in memory before reading into R. Make sure your computer has enough memory (most modern computers should be fine).
# - We will only be using data from the dates 2007-02-01 and 2007-02-02. 
#   One alternative is to read the data from just those dates rather than reading in the entire dataset and subsetting to those dates.
# - You may find it useful to convert the Date and Time variables to Date/Time classes in R using the strptime() and as.Date() functions.
# - Note that in this dataset missing values are coded as ?.
library("data.table")

file.dir <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(file.dir,  "./ProjectdataFiles.zip")
unzip(zipfile = "ProjectdataFiles.zip")

# household_power_consumption.txt is in the zip file.

#########################################################
# Use the alternative way: read the data from just those dates 2007-02-01 and 2007-02-02

DatesAll <- fread("household_power_consumption.txt",select =c(1),sep=";",header = TRUE)
# SelectDates <- which(strptime(DatesAll$Date,"%d/%m/%Y")=="2007-02-01"|strptime(DatesAll$Date,"%d/%m/%Y")=="2007-02-02")
SelectDates <- which(DatesAll$Date=="1/2/2007"|DatesAll$Date=="2/2/2007")
DataHeader <- colnames(fread("household_power_consumption.txt",nrows=1,sep=";",header = TRUE))
# missing values as "?" to NA
SelectedData.list <- lapply(SelectDates,function(x){
  fread("household_power_consumption.txt",skip=x,nrows=1,header = FALSE,sep=";",na.strings="?")}) 
# convert list to a matrix
SelectedData <- data.frame(t(matrix(unlist(SelectedData.list),ncol = length(SelectDates)))) 
colnames(SelectedData) <- DataHeader
# convert factor to character or numeric
SelectedData[,1:2] <- sapply(SelectedData[,1:2],as.character)
SelectedData[,3:9] <- sapply(SelectedData[,3:9],function(x){as.numeric(as.character(x))})
  

# convert the Date and Time variables to Date/Time classes 
DateTime.list <- strptime(paste(SelectedData[,1],SelectedData[,2]),"%d/%m/%Y %H:%M:%S")
SelectedData$DateTime <- DateTime.list
#############################
# Plot
# plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.

# plot the frequency of Global Active Power(kilowatts), title: Global Active Power
png(filename = "plot1.png",
    width = 480, height = 480, units = "px")
with(SelectedData,hist(Global_active_power,col="red",xlab = "Global Active Power (kilowatts)",
     main=gsub("_"," ",DataHeader[3])))
dev.off()




