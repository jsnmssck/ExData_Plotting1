plot1 <- function() {
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
        # Create histogram
        with(
                dat,
                hist(
                        Global_active_power,
                        main = "Global Active Power",
                        xlab = "Global Active Power (kilowatts)",
                        col = "red"
                )
        )
        
        # Copy to PNG
        dev.copy(
                png,
                filename = "plot1.png",
                width = 480,
                height = 480,
                unit = "px"
        )
        dev.off()
}