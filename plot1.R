# File: plot1.R
# Purpose: Imports dataset of electric power consumption and creates a chart
#       showing frequency of global active power. The chart is saved as a png
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

# DrawChart1()
# Purpose: Displays a histogram of frquency of global active power.
DrawChart1 <- function() {
        
        # create var for easier readability
        global_active_power <- powerSubset$Global_active_power
        
        # reset display to one row, one column
        par(mfrow=c(1, 1))
        
        hist(global_active_power, 
             col="#FF2500", 
             main="Global Active Power", 
             xlab="Global Active Power (kilowatts)", 
             ylab="Frequency")
        
} # end DrawChart1()

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
png(file="plot1.png", width=480, height=480, units="px")
DrawChart1()
dev.off()

# display chart in screen device
DrawChart1()
