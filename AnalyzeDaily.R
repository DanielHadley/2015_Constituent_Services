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
library(lubridate)

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
#### Thorough analysis of one work order type ####

d$Service.Type <- gsub("/", "-", d$Service.Type) # take out / because it makes it hard to save plots

# First look at work order to see what to put below
sort(unique(d$Service.Type))

# Then copy and paste it into the quotes below and run the following code
workOrder <- "Health-Snow-Sidewalk not Shoveled"

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



#### Maps ####

workOrderDataRecent <- filter(workOrderData, DaysAgo >= -30)

addresses <- paste(workOrderDataRecent$Location, "Somerville", "MA", sep=", ")
locs <- geocode(addresses)
locs2 <- subset(locs, lat != 42.3875968 ) # Takes out the weird ones Google couldn't pin
# I map locs2 because when Google can't find something, it usually puts it int the center of the map
# This throws off the heat maps


# Dot map 
map.center <- geocode("Central Rd, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 14)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("East Somerville, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_East.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("West Somerville, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_West.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("Central Rd, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_Central.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("Union Sq, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 16)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 4, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_Union.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("Davis Sq, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 16)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 4, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_Davis.png", sep=""), dpi=250, width=6, height=5)



# More traditional heat map
map.center <- geocode("Central Rd, Somerville, MA")
map.center <- c(lon=map.center$lon, lat=map.center$lat)
somerville.map = get_map(location = map.center, zoom = 13, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% locs2 + aes(x = locs2$lon, y = locs2$lat) +
  # geom_density2d(data = locs2, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = locs2, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))


ggsave(paste("./plots/OneOff/",workOrder, "_map_Heat1.png", sep=""), dpi=250, width=6, height=5)


# More traditional heat map
map.center <- geocode("Central Rd, Somerville, MA")
map.center <- c(lon=map.center$lon, lat=map.center$lat)
somerville.map = get_map(location = map.center, zoom = 14, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% locs2 + aes(x = locs2$lon, y = locs2$lat) +
  # geom_density2d(data = locs2, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = locs2, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))


ggsave(paste("./plots/OneOff/",workOrder, "_map_Heat2.png", sep=""), dpi=250, width=6, height=5)

