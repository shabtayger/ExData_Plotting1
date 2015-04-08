library(sqldf)


plot3 <- function() {
  # Download and unzip the elictrical power usage data file from the web, 
  # Generates and saves a png image which contains a specified grapph.
  # Before exiting, the temp directory is removed
  #
  # Args: None
  #
  # Returns: The access time to the web site
  
  # Side effects:
  #   The function saves a png image in the working directory
  #
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  # File access time is recorded. 
  accessTime <- format(Sys.time(), "%Y%m%d_%H%M%S%Z")
  # Locale must be set. Otherwize, the graph will display the dates in the local locale format
  Sys.setlocale("LC_TIME", "C")
  filenames <- downloadAndUnzip(url)
  data <- loadData(filenames["data"]$data)
  plot3x(data)
  # Clear the data file and the temp directory
  unlink(filenames["zipDir"], recursive = TRUE, force = TRUE)
  accessTime
}

plot3x <- function(data) {
  # Generates plot1 and saves it as a png image in the working directory
  #
  # Args: 
  #   data: the data frame which includes the power data
  #
  # Returns: Nothing
  #
  # Side effects:
  #   The function saves the graph as plot3.png image in the working directory
  #
  png(file = "plot3.png")
  par(mfrow = c(1, 1))
  with(data, plot(Time, Sub_metering_1, ylab = "Energy sub metering", col = "black",
                  xlab="", type = "l"))
  with(data, points(Time, Sub_metering_2, col = "red", type='l'))
  with(data, points(Time, Sub_metering_3, col = "blue", type='l'))
  legend("topright", pch = 1, col = c("black", "blue", "red"), cex=0.7,
            legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  dev.off()
}

loadData <- function(filename) {
  # Generates a data frame which includes only the power usage data from 1/2/2007-2/2/2007
  # Set the Date and Time columns to datetime format
  #
  # Args: 
  #   filename: filename which includes the data
  #
  # Returns: A data frame with the data 
  #
  # Side effects: None
  #
  data <- read.csv.sql(filename, sep=';', header=TRUE, 
                  sql = "select * from file where Date = '1/2/2007' or Date = '2/2/2007' ")
  data$Time<-strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
  data$Date<-as.Date(data$Date , "%d/%m/%Y")
  data
}

downloadAndUnzip <- function(url) {
  # Download and unzip the input data file feom the web, save it in a temp directory and unzip it.
  # The function verifies that the zip file structure is as expected
  #
  # Args:
  #   url: The URL of the original data file
  #
  # Returns: A list with the full paths of the temp directory and the unzipped data file
  #
  # Side effects:
  #   The function creates files in temp directories
  #
  zipfile <- tempfile()
  download.file(url = url, destfile = zipfile, quiet = TRUE)
  zipdir <- tempfile()
  dir.create(zipdir)
  unzip(zipfile, exdir = zipdir) # files="" so extract all
  file.remove(zipfile)
  files <- list.files(zipdir)
  if (length(files) > 1 || files[1] != "household_power_consumption.txt")
    stop("wrong Dataset. household_power_consumption.txt is expected.")
  datafile <- paste(zipdir, files[1], sep="/")
  lst <-list(zipDir= zipdir, data = datafile)
}

plot3()