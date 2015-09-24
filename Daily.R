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
library(httr) # Upload to Socrata


#### Load Data & Update via QSend API ####
# I leave out reqcustom, attachment, and deleted
activity <- read.csv("./data/activity.csv")
submitter <- read.csv("./data/submitter.csv")
request <- read.csv("./data/request.csv")
reqcustom <- read.csv("./data/reqcustom.csv")


# Changes since x
# I do five days ago in case there is a problem for one or two days with the system
since <- Sys.Date() - 5

api <- paste("https://somervillema.qscend.com/qalert/api/v1/requests/changes/?since=", month(since), "%2F", day(since), "%2F", year(since), "&key=5c2b987d13cc414cb26f956cf31fbffc8ca62dc37d1a4f6bba3cc74398162db5", sep = "")

d <- fromJSON(api)

activityChanges <- d$activity
submitterChanges <- d$submitter
requestChanges <- d$request
reqcustomChanges <- d$reqcustom


# Now merge the dataframes

# Merge and get rid of dupes for activity
activityUpdated <- rbind(activity, activityChanges)
activityUpdated <- distinct(activityUpdated)

reqcustomUpdated <- rbind(reqcustom, reqcustomChanges)
reqcustomUpdated <- distinct(reqcustomUpdated)

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

write.csv(reqcustomUpdated, "//fileshare1/Departments2/Somerstat Data/Constituent_Services/data/reqcusreqcustom.csv", row.names = FALSE)
write.csv(reqcustomUpdated, "./data/reqcusreqcustom.csv", row.names = FALSE)


# Remove everything else
remove(activity, activityChanges, request, requestChanges, submitter, submitterChanges, reqcustom, reqcustomChanges, d)


#### Prepare a singe datset for upload to Socrata and elsewhere ####
# the summarise is the way to get the latest action by using which.max
lastAction <- activityUpdated  %>%
  filter(codeDesc != "Printed" & codeDesc != "Escalated" & codeDesc != "Submitter Contacted") %>% 
  group_by(requestId) %>% 
  summarise(LastAction = codeDesc[which.max(id)],
            dateLastAction = displayDate[which.max(id)])


d <- merge(requestUpdated, lastAction, by.x = "id", by.y = "requestId")

#### Narrow down to useful columns for saving in various locations ####
# I drop displayLastAction because it is not the same as the date I create above
# Because above I take out things like "printed"
# Who cares when it was printed?!? That's not an action
d <- d %>% 
  select(id, cityName, comments, dept, displayDate, district, latitude, longitude, streetId: dateLastAction)


# Create a more general 'type' column from the #s given to me by S. Craig
# Call it the weird name because that is a socrata convention
serviceRequests <- c(269, 422,424,492,425,503, 504,427,428,272,417,418,475,419,420,421, 273,274,493,494,271, 495,496,413,414,415,502,497,498,471,499,500,501,506,507,508,509,510,511,505,512,275,580,482,315,316, 317,276,466,299,301,488,302,303,304,305,307,308,277,310,311,594,467,483,484,278,322,437,438,439,440,441,442,443,338,444,445,446,447,340,448,449,450,451,470,452,341,453,454,456,455,457,458,459,460,461,462,464,465,463,339,280,360,346,347,348,349,361,364,318,350,351,352,353,366,365,386,367,358,402663,370,371,369,281,289,290,294,293,295,282,284,550,588,400324,435,287)

informationCalls <- c(526,373,378,581,527,528,589,591,532,579,582,530,534,587,531,412,411,381,382,533,535,536,539,540,542,541,578,400135,374,400049,401190,400445,401978,389,390,391,392,393,376,400127,394,395,396,544,546,398,399,402,403,408,409,543,547,548,404,400074,400604,549,401953,401775,401951,586,597,596,400464,598,400466,599,600,590,603,604,605,606,601,602,400254,552,553,554,555,556,557,559,560,570,563,564,565,566,567,568,569,561,571,572,573)

DPWInternal <- c(473,476,474,402500,475,481,477,487,478,470,480)

d$secondary_issue_type <- ifelse(d$typeId %in% serviceRequests, "Service Requests", 
                 ifelse(d$typeId %in% informationCalls, "information calls",
                        ifelse(d$typeId %in% DPWInternal, "DPW Internal", NA)))


# Write it out
write.csv(d, "//fileshare1/Departments2/Somerstat Data/Constituent_Services/data/311_Somerville.csv", row.names = FALSE)
write.csv(d, "./data/311_Somerville.csv", row.names = FALSE)




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
  filter(DaysAgo > -2) %>%
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


