library(dplyr)
library(sqldf)

# Retrieve and unzip the data file if we don't have it already
zip.file.url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zip.file.local<-"power_consumption.zip"
if (!file.exists(zip.file.local)) {
  message("Downloading zip file")
  download.file(zip.file.url,zip.file.local,mode="wb")
}
if (!file.exists(zip.file.local)) {
  stop("Failed to retrieve zip file!")
}
message("Unzipping data file")
unzip(zip.file.local)

# When reading the data file, only keep the records that we're interested in
rawfile<-"household_power_consumption.txt"
message("Reading data file")
mydata<-read.csv.sql(rawfile,sql="select * from file where Date in ('1/2/2007','2/2/2007')",header=TRUE,sep=";")
closeAllConnections()

# Convert data frame to table frame for dplyr;
message("Preparing data file")
mydf<-tbl_df(mydata) %>%
  # The 'Date' and 'Time' fields are characters, convert them to a single Time field
  mutate(DateTime = as.POSIXct(strptime(paste(Date,Time),format="%d/%m/%Y %H:%M:%S")))

# Clean up unnecessary files and variables
message("Cleaning temp items")
unlink("household_power_consumption.txt")
rm(mydata)
rm(rawfile)
rm(zip.file.url)
rm(zip.file.local)
message("Data loaded, ready for processing")
