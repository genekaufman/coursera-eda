# plot5.R - How have emissions from motor vehicle sources changed from
#           1999-2008 in Baltimore City?
#
# My Approach:  filter out Baltimore, Motor Vehicle data, Group by year, add
#               trendline to indicate change in emissions; line sloping down
#               (left-to-right) shows a decrease in emissions, while a line
#               sloping up shows an increase.
thisPlotName <- "plot5"

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
# pull out Motor sources - best indicated by "Mobile" in SCC.Level.One
SCCL1_Mobile<-SCC[grepl("Mobile",SCC$SCC.Level.One),]

# Filter out Baltimore
NEI_Balt <- filter(NEI,fips == "24510")

# create a new joined data frame that includes only those
# observations in NEI that match the filtered sources in
# SCCL1_Mobile
# Since EI.Sector has a dot in it, it must be enclosed in quotes
# or else sqldf will try to interpret it as table.column
join_sql <- "select NEI_Balt.*, SCCL1_Mobile.'EI.Sector'
                from NEI_Balt
                join SCCL1_Mobile
                on NEI_Balt.SCC = SCCL1_Mobile.SCC"

joined_summary <- sqldf(join_sql,stringsAsFactors=FALSE) %>%
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions)/1000)

message("Creating plot")
# Time to actually create the graph

# get the range for the x and y axis
xrange <- range(joined_summary$year)
te.max <- max(joined_summary$total_emissions)
te.min <- min(joined_summary$total_emissions)
# set the vertical scale 10% below the min and 10% above the max
yrange<-c(te.min * 0.9 ,te.max * 1.1)
# get the trendline data
fit <- lm(total_emissions~year, data=joined_summary)

# create a blank plot
plot(xrange, yrange,
     type="n",
     main="PM2.5 Total Emissions By Year\nMotor Vehicle Sources, Baltimore",
     xaxt = "n",
     xlab="",
     ylab="Total Emissions, Tons (x1000)"
)
# clean up x-axis labels
axis(side=1,
     at=unique(joined_summary$year))

# add emission plot
lines(joined_summary$year,joined_summary$total_emissions,type="l",col="red",lwd=3)

# add trendline
abline(fit,col="blue")

# clear out temp variables and data
message("Clearing assignment-specific temp items")
rm(list=ls()[ls()!="NEI" & ls()!="SCC" & ls()!="pngOutput"])


# closes the png device if necessary
if (pngOutput) {
  dev.off()
}

