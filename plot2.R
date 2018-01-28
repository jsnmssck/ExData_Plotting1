plot2 <- function() {
        # Read full data
        fulldat <- read.table(
                "./household_power_consumption.txt",
                header = TRUE,
                sep = ";",
                na.strings = "?"
        )
        
        # Subset to relevant days
        dat <-
                subset(fulldat, Date == "1/2/2007" |
                               Date == "2/2/2007")
        
        # process Date and Time
        dat$DateTimeStr <- paste(dat$Date, dat$Time, sep = " ")
        dat$DateTime <-
                as.POSIXlt(dat$DateTimeStr, format = "%d/%m/%Y %H:%M:%S")
        
        
        ## Create line graph
        
        # make empty plot
        with(
                dat,
                plot(
                        DateTime,
                        Global_active_power,
                        type = "n",
                        xlab = "",
                        ylab = "Global Active Power (kilowatts)"
                )
        )
        
        # add line
        with(dat,
             lines(DateTime, Global_active_power, type = "l"))
        
        # Copy to PNG
        dev.copy(
                png,
                filename = "plot2.png",
                width = 480,
                height = 480,
                unit = "px"
        )
        dev.off()
}