plot1 <- function(file, skipRows, numOfRows, separator) {
    # Grabbing the header info and only
    header <- read.table(file, nrows = 1, header = FALSE, sep = separator, stringsAsFactors = FALSE)
    # Ignoring and reading as many lines as the given arguments 
    data <- read.table(file, skip=skipRows, nrows=numOfRows, sep = separator)
    # Name the headers
    colnames( data ) <- unlist(header)
    # Create the graph
    hist(data$Global_active_power, xlab = "Global Active Power (kilowatts)", col = "red", main = "Global Active Power")
    # Copy the created graph to the file
    dev.copy(png, width=480, height=480, file="plot1.png")
    # Close it
    dev.off()
}