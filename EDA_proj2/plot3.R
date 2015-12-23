# plot3.R - Of the four types of sources indicated by the type (point, nonpoint,
#           onroad, nonroad) variable, which of these four sources have seen
#           decreases in emissions from 1999-2008 for Baltimore City? Which have
#           seen increases in emissions from 1999-2008? Use the ggplot2 plotting
#           system to make a plot answer this question.
#
# My Approach:  filter out Baltimore data, group by year, type, add trendline
#               to indicate change in emissions; line sloping down (left-to-right)
#               shows a decrease in emissions, while a line sloping up shows an
#               increase.
#
thisPlotName <- "plot3"

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
# filter out Baltimore
yearlyEmissionsBalt <- filter(NEI,fips == "24510") %>%
  group_by(year,type) %>%
  summarize(total_emissions = sum(Emissions)/1000)

message("Creating plot")
# Time to actually create the graph

#get the x-axis year labels
xvals <- unique(yearlyEmissionsBalt$year)

library(ggplot2)
# create ggplot
thisplot<-ggplot(yearlyEmissionsBalt, aes(year,total_emissions)) +
  geom_line() +
  facet_grid(~type) +
  ylab("Total Emissions")+
  ggtitle("Baltimore Emissions by Type") +
  stat_smooth(method="lm",se=FALSE) +
  scale_x_continuous(breaks=xvals,minor_breaks=NULL) +
  theme(panel.margin = unit(1, "lines"))

print(thisplot)

# clear out temp variables and data
message("Clearing assignment-specific temp items")
rm(list=ls()[ls()!="NEI" & ls()!="SCC" & ls()!="pngOutput"])

# closes the png device if necessary
if (pngOutput) {
  dev.off()
}


