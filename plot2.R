#-----------------------------------------------------------------------------------#
#            Course Project 1 - Electric Power Consumption                          #
#-----------------------------------------------------------------------------------#
setInternet2(TRUE)
library(data.table)
library(lubridate)
library(dplyr)

#-----------------------------------------------------------------------------------#
#            Import and format Data Set                                             #
#-----------------------------------------------------------------------------------#
##  Download file
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"    

if(!file.exists("Household_Power_Consumption.zip")){
    download.file(fileUrl,"Household_Power_Consumption.zip")}

##  Unzip and read into R - format as data frame to avoid data table format/class issues
dat <- unzip("Household_Power_Consumption.zip")
hpc001 <- as.data.frame(fread(dat, na.strings="?"))

##  Select only required dates
hpc002 <- hpc001[HPC001$Date == "1/2/2007" | HPC001$Date == "2/2/2007",]

##  Assign correct column classes / formats
hpc003 <- hpc002
hpc003[,3:9] <- lapply(hpc002[,3:9], as.numeric)
hpc003$Date <- dmy(hpc002$Date) 

##  Create date time and weekday columns
hpc003$date.time <- ymd_hms(paste(hpc003$Date,hpc003$Time))
hpc003$wk.day <- wday(hpc003$Date, label=TRUE, abbr=TRUE)

data <- hpc003


#-----------------------------------------------------------------------------------#
#            Exploratory Analysis                                                   #
#-----------------------------------------------------------------------------------#
##  Plot 2 - Global Active Power by Weekday
plot2 <- function(){
    with(data,
    plot(Global_active_power ~ date.time,
        col="Black",
         type="l",
         ylab="Global Active Power (kilowatts)",
         xlab=""))
}


#-----------------------------------------------------------------------------------#
#            Export Graph                                                           #
#-----------------------------------------------------------------------------------#
##  Set up ploting area
par(mfrow=c(1,1))

##  Produce plots in desired order
plot2()

##  Copy graph from screen device to png format
dev.copy(png, file="plot2.png")
dev.off()
