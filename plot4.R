#-----------------------------------------------------------------------------------#
#            Course Project 1 - Electric Power Consumption                          #
#-----------------------------------------------------------------------------------#
setwd("C:/Users/mityu_000/Documents/Coursera/Data Science Specialisation/04. Exploratory Data Analysis")
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
##  Plot 3 - Energy Sub Metering
plot3 <- function(){
    with(data, 
    plot(Sub_metering_1 ~ date.time, 
        col="Black", 
        type="l",
        xlab="",
        ylab="Energy sub metering"))  

with(data, lines(Sub_metering_2 ~ date.time, col="Red", type="l"))
with(data, lines(Sub_metering_3 ~ date.time, col="Blue", type="l"))

legend("topright", lty=1, col=c("Black","Red", "Blue"), bty="n", cex=0.75,
        legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
}

##  Plot 4 - Voltage
plot4 <- function(){
    with(data,      
    plot(Voltage ~ date.time, 
        col="Black", 
        type="l"))
}

##  Plot 5 - Global Reactive Power
plot5 <- function(){
    with(data,
    plot(Global_reactive_power ~ date.time,
        col="Black",
        type="l"))
}


#-----------------------------------------------------------------------------------#
#            Export Graph                                                           #
#-----------------------------------------------------------------------------------#
##  Set up ploting area for 2x2
par(mfrow=c(2,2))

##  Produce plots in desired order
plot2()
plot4()
plot3()
plot5()

##  Copy graph from screen device to png format
dev.copy(png, file="plot4.png")
dev.off()




