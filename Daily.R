#### Created 10/14 by Daniel Hadley to load and analyze 311 Data ####
# The FTP site was created by Ahmod at Intelligov


# working Directory and packages #
setwd("k:/Somerstat/Common/Data/2015_Constituent_Services/")
setwd("c:/Users/dhadley/Documents/GitHub/2015_Constituent_Services")


library(RCurl)
library(plyr)
library(ggplot2)
library(scales) # for changing from scientific notation
# Example scale feature: scale_y_continuous(labels = comma) or scale_y_continuous(labels = dollar)
library(reshape2)



#### Download Daily DATA ####
# First retreive the monthly data from the ftp server maintained by Intelligov
# This has all the work orders/quick tickets, and data on how quickly they are completed
url<-c("sftp://somervillemadata@ftp2.ciacorp.com/311DailyAllMonthDataDump.csv")
x <-getBinaryURL(url, userpwd="somervillemadata:today123")

# Write it 
# http://stackoverflow.com/questions/18833031/download-rdata-and-csv-files-from-ftp-using-rcurl-or-any-other-method
writeBin(x, "./data/Daily.csv")

# Read it
# I had problems with the encoding, so I added the f <- file 
# http://stackoverflow.com/questions/4806823/how-to-detect-the-right-encoding-for-read-csv
f <- file('./data/Daily.csv', open="r", encoding="UTF-16LE")

# Turn it into a dataframe
# At first I got an error, so I added the fill=TRUE 
# http://stackoverflow.com/questions/18161009/error-in-reading-in-data-set-in-r
d <- read.table(f, sep=',', dec='.', header=TRUE, fill = TRUE)


# Remove everything else
remove(f, url, x)


#### Add variables/ Clean Data ####
d$Tab <- 1


####  Visualize ####

lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"


my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    # panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=6), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=12) # Enlarge, left-align title
    #,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )

d.top <- subset(d, Service.Type %in% arrange(count(d, .(Service.Type)), desc(freq))[1:10,]$Service.Type)
toptype <- ddply(d.top, "Service.Type", summarise, count = sum(Tab, na.rm=TRUE))


ggplot(toptype, aes(x=reorder(toptype$Service.Type, -toptype$count), y=toptype$count)) + geom_bar(stat = "identity", colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Calls Since 2007") + xlab("Top Call Types")+ylab("Number of Calls") +
  theme(axis.text.x = element_text(angle=60, hjust = 1))

ggsave("./plots/daily01.png", dpi=300, width=3, height=3)

