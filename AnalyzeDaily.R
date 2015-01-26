#### Created 10/14 by Daniel Hadley to load and analyze 311 Data ####
# The FTP site was created by Ahmod at Intelligov


# working Directory and packages #
setwd("c:/Users/dhadley/Documents/GitHub/2015_Constituent_Services")


library(RCurl)
library(dplyr)
library(ggplot2)
library(scales) # for changing from scientific notation
library(tidyr)


# Read it
# I had problems with the encoding, so I added the f <- file 
# http://stackoverflow.com/questions/4806823/how-to-detect-the-right-encoding-for-read-csv
f <- file('./data/Daily.csv', open="r", encoding="UTF-16LE")

# Turn it into a dataframe
# At first I got an error, so I added the fill=TRUE 
# http://stackoverflow.com/questions/18161009/error-in-reading-in-data-set-in-r
d <- read.table(f, sep=',', dec='.', header=TRUE, fill = TRUE)


# Write it to the K drive
# write.csv(d, "K:/Somerstat/Common/Data/2015_Constituent_Services/data/Daily.csv")


# Remove everything else
remove(f)


# dates

d$Date <- as.Date(d$Date, "%m/%d/%Y")
d$Year.Month <- format(d$Date, '%Y-%m')
d$Month <- format(d$Date, '%m')
d$Year <- format(d$Date, '%Y')




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
    axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )



#### analysis ####


# Potholes

Potholes <- d %>%
  filter(Service.Type == 'DPW-Pothole') %>%
  group_by(Year.Month) %>%
  summarize(count=n())

ggplot(Potholes, aes(x=Year.Month, y=count, group = 1)) + 
  geom_line(colour=lime_green, size = 1.5) + 
  my.theme + ggtitle("Potholes Over Time") + xlab("Year-Month") +
  ylab("count") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/Potholes.png", dpi=300, width=7, height=5)




First <- d %>%
  group_by(Service.Type) %>%
  summarise(FirstDate = min(Date)) %>%
  arrange(FirstDate)
