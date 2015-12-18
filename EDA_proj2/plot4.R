# plot4.R - Across the United States, how have emissions from coal combustion-related
# sources changed from 1999-2008?
#
##### Data Retrieval/Loading - Standard between all plots #######
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
  message("Cleaning temp items")
  unlink(raw.file.scc)
  unlink(raw.file.summary)
  rm(raw.file.scc)
  rm(raw.file.summary)
  rm(zip.file.url)
  rm(zip.file.local)
  message("Data loaded, ready for processing")

}

# set pngOutput to false to write to screen
pngOutput<-FALSE
if (pngOutput) {
  png(file="plot4.png")
}

# filter out Baltimore, apply same data massaging as plot1
yearlyEmissionsBalt <- filter(NEI,fips == "24510") %>%
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions)/1000)

# pull out Coal sources
SCCEIS_Coal<-SCC[grepl("Coal",SCC$EI.Sector),]

# create a new joined data frame
join_sql <- "select NEI.*, SCCEIS_Coal.*
                from NEI
                join SCCEIS_Coal
                on NEI.SCC = SCCEIS_Coal.SCC"

joined_tbl <- sqldf(join_sql,stringsAsFactors=FALSE)




message("Creating plot")
# Time to actually create the graph

# get the range for the x and y axis
xrange <- range(joined_tbl$year)
yrange <- range(joined_tbl$total_emissions)

# create a blank plot
plot(xrange, yrange,
     type="n",
     main="PM2.5 Total Emissions By Year - Coal Combustion",
     xaxt = "n",
     xlab="",
     ylab="Total Emissions, Tons (k)"
)

axis(side=1,
     at=unique(joined_tbl$year))

lines(joined_tbl$year,joined_tbl$total_emissions,type="l",col="red",lwd=5)

rm(xrange)
rm(yrange)

# closes the png device if necessary
if (pngOutput) {
  dev.off()
}


