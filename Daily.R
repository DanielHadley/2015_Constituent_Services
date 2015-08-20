#### Created 10/14 by Daniel Hadley to load and analyze 311 Data ####
# Updated 8/15 for Qsend


# working Directory and packages #
setwd("c:/Users/dhadley/Documents/GitHub/2015_Constituent_Services")


library(RCurl)
library(dplyr)
library(ggplot2)
library(scales) # for changing from scientific notation
library(tidyr)
library(jsonlite)
library(lubridate)


#### Load Data & Update via QSend API ####
# I leave out reqcustom, attachment, and deleted
activity <- read.csv("./data/activity.csv")
submitter <- read.csv("./data/submitter.csv")
request <- read.csv("./data/request.csv")


# Changes since x
# I do five days ago in case there is a problem for one or two days with the system
since <- Sys.Date() - 5

api <- paste("https://somervillema.qscend.com/qalert/api/v1/requests/changes/?since=", month(since), "%2F", day(since), "%2F", year(since), "&key=5c2b987d13cc414cb26f956cf31fbffc8ca62dc37d1a4f6bba3cc74398162db5", sep = "")

d <- fromJSON(api)

activityChanges <- d$activity
submitterChanges <- d$submitter
requestChanges <- d$request


# Now merge the dataframes

# Merge and get rid of dupes for activity
activityUpdated <- rbind(activity, activityChanges)
activityUpdated <- distinct(activityUpdated)

# Overwrite for request & submitter
# A clever method: 
# http://stackoverflow.com/questions/28282484/join-two-dataframes-and-overwrite-matching-rows-r
requestUpdated <- rbind(requestChanges, request[!request$id %in% requestChanges$id,])
submitterUpdated <- rbind(submitterChanges, submitter[!submitter$id %in% submitterChanges$id,])


#### Write it ####

# Write it to the P: drive and my local
write.csv(requestUpdated, "//fileshare1/Departments2/Somerstat Data/Constituent_Services/data/request.csv", row.names = FALSE)
write.csv(requestUpdated, "./data/request.csv", row.names = FALSE)

write.csv(activityUpdated, "//fileshare1/Departments2/Somerstat Data/Constituent_Services/data/activity.csv", row.names = FALSE)
write.csv(activityUpdated, "./data/activity.csv", row.names = FALSE)

write.csv(submitterUpdated, "//fileshare1/Departments2/Somerstat Data/Constituent_Services/data/submitter.csv", row.names = FALSE)
write.csv(submitterUpdated, "./data/submitter.csv", row.names = FALSE)


# Remove everything else
remove(activity, activityChanges, request, requestChanges, submitter, submitterChanges, d)



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

requestUpdated$Date <- as.Date(requestUpdated$displayDate, "%m/%d/%Y")
requestUpdated$Year.Month <- format(requestUpdated$Date, '%Y-%m')
requestUpdated$Month <- format(requestUpdated$Date, '%m')
requestUpdated$Year <- format(requestUpdated$Date, '%Y')

requestUpdated$DaysAgo <- difftime(requestUpdated$Date, today, units = "days")




#### Top from last day #### 

LastTwentyFour <- requestUpdated %>%
  filter(DaysAgo > -1) %>%
  group_by(typeName) %>%
  summarize(count=n()) %>%
  filter(count > 5)

ggplot(LastTwentyFour, aes(x=reorder(typeName, count)  , y=count)) + 
  geom_bar(stat = "identity", colour="white", fill=nice_blue) + 
  my.theme + ggtitle(paste("Top Work Orders From Yesterday:", yesterday)) + xlab("Request") +
  ylab("# of Requests") + 
  scale_y_continuous(labels = comma) 

# ggsave(paste("./plots/daily/", yesterday, "_LastTwentyFour.png", sep=""), dpi=300, width=5, height=5)
ggsave("./plots/daily/LastTwentyFour.png", dpi=300, width=5, height=5)
ggsave("//fileshare1/Departments2/Somerstat Data/Constituent_Services/plots/LastTwentyFour.png", dpi=300, width=5, height=5)







# Footnotes
# #### The initial Data dump ####
# # QSend API undocumented method to get all data
# 
# api <- "https://somervillema.qscend.com/qalert/api/v1/requests/dump/?start=7%2F1%2F2015&key=5c2b987d13cc414cb26f956cf31fbffc8ca62dc37d1a4f6bba3cc74398162db5"
# 
# d <- fromJSON(api)
# 
# request <- d$request
# activity <- d$activity
# attachment <- d$attachment
# submitter <- d$submitter
# deleted <- d$deleted
# reqcustom <- d$reqcustom
# 
# write.csv(request, "./data/2015_08_20_Qalert_Data_Dump/request.csv", row.names = FALSE)
# write.csv(activity, "./data/2015_08_20_Qalert_Data_Dump/activity.csv", row.names = FALSE)
# write.csv(attachment, "./data/2015_08_20_Qalert_Data_Dump/attachment.csv", row.names = FALSE)
# write.csv(submitter, "./data/2015_08_20_Qalert_Data_Dump/submitter.csv", row.names = FALSE)
# write.csv(deleted, "./data/2015_08_20_Qalert_Data_Dump/deleted.csv", row.names = FALSE)
# write.csv(reqcustom, "./data/2015_08_20_Qalert_Data_Dump/reqcustom.csv", row.names = FALSE)


