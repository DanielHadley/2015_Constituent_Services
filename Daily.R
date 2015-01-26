#### Created 10/14 by Daniel Hadley to load and analyze 311 Data ####
# The FTP site was created by Ahmod at Intelligov


# working Directory and packages #
setwd("c:/Users/dhadley/Documents/GitHub/2015_Constituent_Services")


library(RCurl)
library(dplyr)
library(ggplot2)
library(scales) # for changing from scientific notation
library(tidyr)



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


# Write it to the K drive
write.csv(d, "//fileshare1/Departments/Somerstat/Common/Data/2015_Constituent_Services/raw_data/Daily.csv")


# Remove everything else
remove(f, url, x)



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
    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )


# dates
today <- Sys.Date()
yesterday <- today - 1

d$Date <- as.Date(d$Date, "%m/%d/%Y")
d$Year.Month <- format(d$Date, '%Y-%m')
d$Month <- format(d$Date, '%m')
d$Year <- format(d$Date, '%Y')

d$DaysAgo <- difftime(d$Date, today, units = "days")




#### Top from last day #### 

LastTwentyFour <- d %>%
  filter(DaysAgo > -2) %>%
  group_by(Service.Type) %>%
  summarize(count=n()) %>%
  filter(count > 5)

ggplot(LastTwentyFour, aes(x=reorder(Service.Type, count)  , y=count)) + 
  geom_bar(stat = "identity", colour="white", fill=nice_blue) + 
  my.theme + ggtitle(paste("Top Work Orders From Yesterday:", yesterday)) + xlab("Request") +
  ylab("# of Requests") + 
  scale_y_continuous(labels = comma) 

# ggsave(paste("./plots/daily/", yesterday, "_LastTwentyFour.png", sep=""), dpi=300, width=5, height=5)
ggsave("./plots/daily/LastTwentyFour.png", dpi=300, width=5, height=5)
ggsave("//fileshare1/Departments/Somerstat/Common/Data/2015_Constituent_Services/plots/LastTwentyFour.png", dpi=300, width=5, height=5)

