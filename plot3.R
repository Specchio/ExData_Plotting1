plot3 <- function(file, skipRows, numOfRows, separator) {
    # Grabbing the header info and only
    header <- read.table(file, nrows = 1, header = FALSE, sep = separator, stringsAsFactors = FALSE)
    # Ignoring and reading as many lines as the given arguments 
    data <- read.table(file, skip=skipRows, nrows=numOfRows, sep = separator)
    # Name the headers
    colnames( data ) <- unlist(header)
    # Create the graph
    data$timestamp <- paste(data$Date, data$Time)
    data$DateTime <- as.POSIXct(data$timestamp, format="%d/%m/%Y %H:%M:%S")
    daterange=c(as.POSIXlt(min(data$DateTime)), as.POSIXlt(max(data$DateTime)))
    
    with(data, plot(DateTime, Sub_metering_1, type="n", ylab="Energy sub metering", xlab = ""))
    lines(data$DateTime, data$Sub_metering_1, type="l")
    lines(data$DateTime, data$Sub_metering_2, col="red")
    lines(data$DateTime, data$Sub_metering_3, col="blue")
    legend("topright", pch="-", lty = 1, lwd = 2, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), inset = 0)
    
    # Copy the created graph to the file
    dev.copy(png, width=480, height=480, file="plot3.png")
    # Close it
    dev.off()
}