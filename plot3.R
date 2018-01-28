plot3 <- function() {
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
                        Sub_metering_1,
                        type = "n",
                        xlab = "",
                        ylab = "Energy sub metering"
                )
        )
        
        # add legend
        legend(
                "topright",
                c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                lty = c(1, 1, 1),
                col = c("black", "red", "blue")
        )
        
        # add line
        with(dat,
             lines(DateTime,
                   Sub_metering_1,
                   type = "l"))
        
        with(dat,
             lines(
                     DateTime,
                     Sub_metering_2,
                     type = "l",
                     col = "red"
             ))
        
        with(dat,
             lines(
                     DateTime,
                     Sub_metering_3,
                     type = "l",
                     col = "blue"
             ))
        
        # Copy to PNG
        dev.copy(
                png,
                filename = "plot3.png",
                width = 480,
                height = 480,
                unit = "px"
        )
        dev.off()
}