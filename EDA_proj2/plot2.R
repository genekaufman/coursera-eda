# plot2.R - Have total emissions from PM2.5 decreased in the 
#           Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#           Use the base plotting system to make a plot answering this question.
#
thisPlotName <- "plot2"

##### Data Retrieval/Loading - Standard between all plots #######
message("*** Starting ", thisPlotName)
# if we don't have the data available, then go get it
if (!exists("NEI")) {
  library(dplyr)
  library(sqldf)

  # Retrieve and unzip the data file if we don't have it already
  zip.file.url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  zip.file.local<-"nei_data.zip"
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
  raw.file.scc<-"Source_Classification_Code.rds"
  raw.file.summary<-"summarySCC_PM25.rds"

  message("Loading data frames")
  ## This first line will likely take a few seconds. Be patient!
  NEI <- readRDS(raw.file.summary)
  SCC <- readRDS(raw.file.scc)

  message("Data massaging")
  NEI$type <- as.factor(NEI$type)
  NEI$SCC <- as.factor(NEI$SCC)
  NEI$fips <- as.factor(NEI$fips)


  # Clean up unnecessary files and variables
  message("Clearing temp items")
  unlink(raw.file.scc)
  unlink(raw.file.summary)
  rm(raw.file.scc)
  rm(raw.file.summary)
  rm(zip.file.url)
  rm(zip.file.local)
  message("Data loaded, ready for processing")

}

# set pngOutput to false to write to screen; snagging pngOutput
# from global environment allows all plot scripts to be ran 
# without having to modify the code here 
if (!exists("pngOutput")) {
  pngOutput<-FALSE
}
pngFilename <- paste0(thisPlotName,".png") 
if (pngOutput) {
  png(file=pngFilename)
  message("Output: ",pngFilename)
} else {
  message("Output: screen")
}

message("Assignment-specific data preparation")
# filter out Baltimore, apply same data massaging as plot1
yearlyEmissionsBalt <- filter(NEI,fips == "24510") %>%
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions)/1000)

message("Creating plot")
# Time to actually create the graph

# get the range for the x and y axis
xrange <- range(yearlyEmissionsBalt$year)
yrange <- range(yearlyEmissionsBalt$total_emissions)

# create a blank plot
plot(xrange, yrange,
     type="n",
     main="PM2.5 Total Emissions By Year - All Sources,\n Baltimore City, MD",
     xaxt = "n",
     xlab="",
     ylab="Total Emissions, Tons (k)"
)

axis(side=1,
     at=unique(yearlyEmissionsBalt$year))

lines(yearlyEmissionsBalt$year,yearlyEmissionsBalt$total_emissions,type="l",col="red",lwd=3)

rm(xrange)
rm(yrange)
rm(yearlyEmissionsBalt)
rm(pngFilename)
rm(thisPlotName)

# closes the png device if necessary
if (pngOutput) {
  dev.off()
}


