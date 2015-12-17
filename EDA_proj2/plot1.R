# plot1.R - Have total emissions from PM2.5 decreased in the United States 
#           from 1999 to 2008? Using the base plotting system, make a plot 
#           showing the total PM2.5 emission from all sources for each of 
#           the years 1999, 2002, 2005, and 2008.
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
  
  # Convert data frame to table frame for dplyr;
  # message("Preparing data file")
  #   df_NEI<-tbl_df(NEI) 
  #   df_SCC<-tbl_df(SCC) 
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
  png(file="plot1.png")
}

# Time to actually create the graph
with(NEI,
     plot(year,Emissions,
          col="red"))


# closes the png device if necessary
if (pngOutput) {
  dev.off()
}


