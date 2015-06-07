plot2 <- function(file, skipRows, numOfRows, separator) {
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
    plot(data$Global_active_power ~ DateTime, data, xaxt = "n", type="l", ylab = "Global Active Power (kilowatts)")
    axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%b %d")
    # Copy the created graph to the file
    dev.copy(png, width=480, height=480, file="plot1.png")
    # Close it
    dev.off()
}