#### Created 10/14 by Daniel Hadley to load and analyze 311 Data ####
# Data is scraped each morning at 6:00 by code on Daniel's pc
# The FTP site was created by Ahmod at Intelligov
# R 3.1+ is needed for this
# If you don't have these packages, install first:

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggmap)

setwd("K:/Somerstat/Common/Data/2015_Constituent_Services")

# Load data
d <- read.csv("./raw_data/Daily.csv")

# Converts data to tbl class. tbl's are easier to examine than data frames. R displays only the data that fits onscreen:
d <- tbl_df(d)

# dates
today <- Sys.Date()
yesterday <- today - 1
sixtyDaysAgo <- today - 60

d$Date <- as.Date(d$Date, "%m/%d/%Y")
d$Year.Month <- format(d$Date, '%Y-%m')
d$Month <- format(d$Date, '%m')
d$Year <- format(d$Date, '%Y')

d$DaysAgo <- difftime(d$Date, today, units = "days")



####  Code to help Visualize in ggplot2 ####

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



#### Start analyzing #### 
# Use this cheat sheet: http://www.rstudio.com/wp-content/uploads/2015/01/data-wrangling-cheatsheet1.pdf

# Get the top work orders from yesterday, plot them, and save to plots
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

ggsave("./plots/LastTwentyFour.png", dpi=300, width=5, height=5)



#### Thorough analysis of one work order type ####

d$Service.Type <- gsub("/", "-", d$Service.Type) # take out / because it makes it hard to save plots

# First look at work order to see what to put below
sort(unique(d$Service.Type))

# Then copy and paste it into the quotes below and run the following code
# I must fix ggsave on ones with / like this:
workOrder <- "DPW-Trash-Xmas Tree Pick Up"

workOrderData <- d %>%
  filter(Service.Type == workOrder)


### Time Series
days <- workOrderData %>%
  group_by(Date) %>%
  summarise(Events = n())

allDays <- seq.Date(from=d$Date[1], to = d$Date[nrow(d)], b='days')
allDays <- allDays  %>%  as.data.frame() 
colnames(allDays)[1] = "Date"

# After this we will have a df with every date and how many work orders
ts = merge(days, allDays, by='Date', all=TRUE)
ts[is.na(ts)] <- 0

remove(allDays, days)

ggplot(ts, aes(x=Date, y=Events)) + 
  geom_line(colour=pinkish_red, size = .5) + 
  my.theme + ggtitle(paste(workOrder, "Calls Over Time")) + xlab("Time") +
  ylab("Daily Calls") + 
  scale_y_continuous(labels = comma)
  
ggsave(paste("./plots/OneOff/",workOrder, "_DailyTimeSeries.png", sep=""), dpi=250, width=5, height=3)


# Now a recent time series
tsr <- ts %>%
  filter(Date > sixtyDaysAgo)

ggplot(tsr, aes(x=Date, y=Events)) + 
  geom_line(colour=pinkish_red, size = .5) + 
  my.theme + ggtitle(paste(workOrder, "Calls Over Time")) + xlab("Time") +
  ylab("Daily Calls") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",workOrder, "_RecentDailyTimeSeries.png", sep=""), dpi=250, width=5, height=3)


# Now a monthly 
tsm <- ts %>%
  mutate(Year.Month = format(Date, '%Y-%m')) %>%
  group_by(Year.Month) %>%
  summarise(Events = sum(Events)) %>%
  mutate(Year.Month = as.Date(paste(Year.Month,1,sep="-"),"%Y-%m-%d"))

ggplot(tsm, aes(x=Year.Month, y=Events, group = 1)) + 
  geom_line(colour=pinkish_red, size = .5) + 
  my.theme + ggtitle(paste(workOrder, "Calls Over Time")) + xlab("Time") +
  ylab("Monthly Calls") + 
  scale_y_continuous(labels = comma) + scale_x_date(labels=date_format("%Y"))

ggsave(paste("./plots/OneOff/",workOrder, "_MonthlyTimeSeries.png", sep=""), dpi=250, width=5, height=3)


# ### Maps
# 
# # Dot map 
# map.center <- geocode("New York City, NY")
# SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 11, color='bw')
# SHmap + geom_point(data=d, aes(y=Latitude, x=Longitude), size = 2, alpha = .7, bins = 26, color="red",) + 
#   ggtitle(paste("Rat Calls: ", lastWeekText, " to ", todayText, ", ", Year, sep="")) 
# 
# ggsave(paste("/Users/dphnrome/Google Drive/RatMaps/posts/NYC_Rat_Map_",today,".png",sep=""), dpi=200, width=4, height=4)