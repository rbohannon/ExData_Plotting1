# File: plot4.R
# Purpose: Imports dataset of electric power consumption and creates four
#       charts displayed 2 x 2. The charts are saved as a png
#       image in the current woring direcetory and displayed to the screen
#       device.
#       
#       NOTE: The data file (household_power_consumption.txt) is expected to 
#               be in a directory named "data" in the current working directory.
#               That is: "./data/household_power_consumption.txt"
#
# Author: Randy Bohannon
# Date: 2014-07-12

###############################################################################

## FUNCTIONS

#******************************************************************************
# DrawChart4()
# Purpose: Displays a four charts 2 x 2:
#       1. Global active power over time
#       2. Voltage over time
#       3. Energy sub-metering over time
#       4. Global reactive power over time
DrawChart4 <- function() {
        
        # create vars for easier readability
        
        date_time <- powerSubset$Date_time
        global_active_power <- powerSubset$Global_active_power
        voltage <- powerSubset$Voltage
        sub_metering_1 <- powerSubset$Sub_metering_1
        sub_metering_2 <- powerSubset$Sub_metering_2
        sub_metering_3 <- powerSubset$Sub_metering_3
        global_reactive_power <- powerSubset$Global_reactive_power

        # set display to 2 rows, 2 columns
        par(mfrow=c(2, 2))
        
        # plot 1:  Global active power over time
        plot(date_time,
             global_active_power, 
             type="l", 
             xlab="",
             ylab="Global Active Power")
        
        
        # plot 2: Voltage over time
        plot(date_time,
             voltage, 
             type="l", 
             xlab="datetime",
             ylab="Voltage")
        
        
        # plot 3: Energy sub-metering over time
        plot(c(date_time, date_time, date_time),
             c(sub_metering_1, sub_metering_2, sub_metering_3), 
             type="n", 
             main="", 
             xlab="", 
             ylab="Energy sub metering")
        
        lines(date_time, sub_metering_1, type="l", col="black")
        lines(date_time, sub_metering_2, type="l", col="red")
        lines(date_time, sub_metering_3, type="l", col="blue")
        
        legend("topright", 
               lty=c(1, 1, 1), # set legend symbols as lines
               lwd=c(1, 1, 1), # set width of lines
               col=c("black", "red", "blue"), 
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        
        # plot 4: Global reactive power over time
        plot(date_time,
             global_reactive_power, 
             type="l", 
             xlab="datetime",
             ylab="Global_reactive_power")
        
        # reset display to one row, one column
        par(mfrow=c(1, 1))
        
} # end Drawchart4()

###############################################################################

# The variable "powerSubset" is a subset of the imported dataset. The chart is
# created using powerSubset. There are other R code files that perform similar 
# functions on powerSubset. The complete dataset is large, so we check if 
# powerSubset already exists before importing the dataset.

# if powerSubset doesn't already exist,
# import the complete dataset and process it into powerSubset
if (!exists("powerSubset")) {
        
        # read in data
        powerData <- read.table("./data/household_power_consumption.txt", 
                                sep=";", 
                                dec=".",
                                header=TRUE, 
                                stringsAsFactors=FALSE, 
                                na.strings="?")
        
        # take subset of data for the days we are interested in
        powerSubset <- powerData[with(powerData, 
                                      Date=="1/2/2007" | Date=="2/2/2007"), ]
        
        # reset row.names
        row.names(powerSubset) <- seq(nrow(powerSubset))
        
        # create a vector containing date/time for each observation
        # by combining Date and Time and converting to Date (POSIXlt)
        # start by creating vector and inserting first date/time
        dt <- c(strptime(paste(powerSubset[["Date"]][1],
                               powerSubset[["Time"]][1]),
                         "%d/%m/%Y %H:%M:%S"))
        
        # repeat for remaining observations
        for (t in 2:2880) {
                dt <- c(dt, strptime(paste(powerSubset[["Date"]][t],
                                           powerSubset[["Time"]][t]),
                                     "%d/%m/%Y %H:%M:%S"))
        }
        
        # create a new column in data subset and insert date/time vector
        powerSubset$Date_time <- dt
        
} # end if

# create png file of chart and save to current working directory
png(file="plot4.png", width=480, height=480, units="px")
DrawChart4()
dev.off()

# display chart in screen device
DrawChart4()
