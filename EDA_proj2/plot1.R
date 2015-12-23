# plot1.R - Have total emissions from PM2.5 decreased in the United States
#           from 1999 to 2008? Using the base plotting system, make a plot
#           showing the total PM2.5 emission from all sources for each of
#           the years 1999, 2002, 2005, and 2008.
#
# My Approach:  Group by year, add trendline to indicate change in emissions;
#               line sloping down (left-to-right) shows a decrease in emissions,
#               while a line sloping up shows an increase.
thisPlotName <- "plot1"

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
### End of code common to all plots ###

message("Assignment-specific data preparation")
yearlyEmissions <- NEI %>%
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions)/1000)

message("Creating plot")
# Time to actually create the graph

# get the range for the x and y axis
xrange <- range(yearlyEmissions$year)
yrange <- range(yearlyEmissions$total_emissions)
# get the trendline data
fit <- lm(total_emissions~year, data=yearlyEmissions)

# create a blank plot
plot(xrange, yrange,
     type="n",
     main="PM2.5 Total Emissions By Year - All Sources",
     xaxt = "n",
     xlab="",
     ylab="Total Emissions, Tons (x1000)"
)
# clean up x-axis labels
axis(side=1,
     at=unique(yearlyEmissions$year))

# add emission plot
lines(yearlyEmissions$year,yearlyEmissions$total_emissions,type="l",col="red",lwd=3)

# add trendline
abline(fit,col="blue")

# clear out temp variables and data
message("Clearing assignment-specific temp items")
rm(list=ls()[ls()!="NEI" & ls()!="SCC" & ls()!="pngOutput"])


# closes the png device if necessary
if (pngOutput) {
  dev.off()
}

