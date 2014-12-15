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
library(dplyr)



#### Download Monthly DATA ####
# First retreive the monthly data from the ftp server maintained by Intelligov
# This has all the work orders/quick tickets, and data on how quickly they are completed
url<-c("sftp://somervillemadata@ftp2.ciacorp.com/311MonthlyAllHistoryDataDump.csv")
x <-getBinaryURL(url, userpwd="somervillemadata:today123")

# Write it 
# http://stackoverflow.com/questions/18833031/download-rdata-and-csv-files-from-ftp-using-rcurl-or-any-other-method
writeBin(x, "./data/Monthly.csv")

# Read it
# I had problems with the encoding, so I added the f <- file 
# http://stackoverflow.com/questions/4806823/how-to-detect-the-right-encoding-for-read-csv
f <- file('./data/Monthly.csv', open="r", encoding="UTF-16LE")

# Turn it into a dataframe
# At first I got an error, so I added the fill=TRUE 
# http://stackoverflow.com/questions/18161009/error-in-reading-in-data-set-in-r
d <- read.table(f, sep=',', dec='.', header=TRUE, fill = TRUE)


#### Add variables/ Clean Data ####
d$Tab <- 1

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
    panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=10), # Enlarge axis text font
    axis.title=element_text(size=12), # Enlarge axis title font
    plot.title=element_text(size=22) # Enlarge, left-align title
    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )

d.top <- subset(d, Service.Type %in% arrange(count(d, .(Service.Type)), desc(freq))[1:10,]$Service.Type)
toptype <- ddply(d.top, "Service.Type", summarise, count = sum(Tab, na.rm=TRUE))


ggplot(toptype, aes(x=reorder(toptype$Service.Type, -toptype$count), y=toptype$count)) + geom_bar(stat = "identity", colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Calls Since 2007") + xlab("Top Call Types")+ylab("Number of Calls") +
  theme(axis.text.x = element_text(angle=60, hjust = 1))

ggsave("./plots/Monthlycalls01.png", dpi=300, width=3, height=3)




#### Monthly overdue ####
overdue <- d %>%
  melt(id=c("Year.Month", "On.Time"), measure=c("Tab")) %>% # id = non-numeric; measure = numeric
  dcast(Year.Month ~ On.Time, sum) %>%
  mutate(Per.On.Time = Y / (Y+N)) %>%
  filter(Y > 2)

ggplot(overdue, aes(x=overdue$Year.Month, y=overdue$Per.On.Time, group=1)) + 
  geom_line(colour=lime_green, size = 1.5) + 
  my.theme + ggtitle("Percent Completed on Time") + xlab("Year-Month") +
  ylab("% On Time") + 
  scale_y_continuous(labels = percent) 

ggsave("./plots/PerOnTme.png", dpi=300, width=7, height=5)


ggplot(overdue, aes(x=overdue$Year.Month, y=overdue$Per.On.Time, group=1)) + 
  geom_line(colour=lime_green, size = 1.5) + 
  my.theme + ggtitle("Percent Completed on Time") + xlab("Year-Month") +
  ylab("% On Time") + 
  scale_y_continuous(labels = percent) +
  expand_limits(y = 0)

ggsave("./plots/PerOnTme2.png", dpi=300, width=7, height=5)



AnnualOverdue <- d %>%
  melt(id=c("Year", "On.Time"), measure=c("Tab")) %>% # id = non-numeric; measure = numeric
  dcast(Year ~ On.Time, sum) %>%
  mutate(Per.On.Time = Y / (Y+N))%>%
  filter(Year > 2007)

ggplot(AnnualOverdue, aes(x=Year, y=Per.On.Time)) + 
  geom_bar(colour="white", fill=lime_green) + 
  my.theme + ggtitle("Percent Completed on Time") + xlab("Year") +
  ylab("% On Time") + 
  scale_y_continuous(labels = percent) +
  expand_limits(y = 0)

ggsave("./plots/PerOnTme3.png", dpi=300, width=5, height=5)



Annual <- d %>%
  melt(id=c("Year", "On.Time"), measure=c("Tab")) %>% # id = non-numeric; measure = numeric
  dcast(Year ~ On.Time, sum) %>%
  mutate(Total = (Y+N+Var.2))%>%
  filter(Year > 2007)

ggplot(Annual, aes(x=Year, y=Total)) + 
  geom_bar(colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Total Work Orders") + xlab("Year") +
  ylab("Work Orders") + 
  scale_y_continuous(labels = comma) +
  expand_limits(y = 0)

ggsave("./plots/PerOnTme4.png", dpi=300, width=5, height=5)



TypesOverdue <- d %>%
  filter(On.Time  == "N") %>%
  group_by(Service.Type) %>%
  summarise(total = sum(Tab)) %>%
  arrange(desc(total)) %>%
  filter(total > 355)

ggplot(TypesOverdue, aes(x=reorder(Service.Type, total), y=total)) + 
  geom_bar(colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Top Past Due") + xlab("Work Orders") +
  ylab("Completed Past Due Since 2008") + 
  scale_y_continuous(labels = comma) +
  coord_flip() 
  
ggsave("./plots/PerOnTme5.png", dpi=300, width=7, height=5)



TypesOverdue <- d %>%
  filter(On.Time  == "N") %>%
  filter(Year  == 2014) %>%
  group_by(Service.Type) %>%
  summarise(total = sum(Tab)) %>%
  arrange(desc(total)) %>%
  filter(total > 55)

ggplot(TypesOverdue, aes(x=reorder(Service.Type, total), y=total)) + 
  geom_bar(colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Top Past Due '14") + xlab("Work Orders") +
  ylab("Completed Past Due 2014") + 
  scale_y_continuous(labels = comma) +
  coord_flip() 

ggsave("./plots/PerOnTme6.png", dpi=300, width=7, height=5)

