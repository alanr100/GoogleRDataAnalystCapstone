#confirm working directory 
getwd()

library(tidyverse) #general covers lots
library(lubridate) #date manipulation
library(dplyr) # statistical help
library(ggplot2) #plotting

# chosen 2022 data
df_01_2022 <- read.csv("CSV/2022/202201-divvy-tripdata.csv", na.strings = c(""))
df_02_2022 <- read.csv("CSV/2022/202202-divvy-tripdata.csv", na.strings = c(""))
df_03_2022 <- read.csv("CSV/2022/202203-divvy-tripdata.csv", na.strings = c(""))
df_04_2022 <- read.csv("CSV/2022/202204-divvy-tripdata.csv", na.strings = c(""))
df_05_2022 <- read.csv("CSV/2022/202205-divvy-tripdata.csv", na.strings = c(""))
df_06_2022 <- read.csv("CSV/2022/202206-divvy-tripdata.csv", na.strings = c(""))
df_07_2022 <- read.csv("CSV/2022/202207-divvy-tripdata.csv", na.strings = c(""))
df_08_2022 <- read.csv("CSV/2022/202208-divvy-tripdata.csv", na.strings = c(""))
df_09_2022 <- read.csv("CSV/2022/202209-divvy-publictripdata.csv", na.strings = c(""))
df_10_2022 <- read.csv("CSV/2022/202210-divvy-tripdata.csv", na.strings = c(""))
df_11_2022 <- read.csv("CSV/2022/202211-divvy-tripdata.csv", na.strings = c(""))
df_12_2022 <- read.csv("CSV/2022/202212-divvy-tripdata.csv", na.strings = c(""))


#check one dataframe to check structure
str(df_01_2022)
summary(df_01_2022)
head(df_01_2022)

#tidy up unwanted columns
unwantedCols <- c(1,6,8,9,10,11,12)
df_01_2022 <- df_01_2022[c(-unwantedCols)]
df_02_2022 <- df_02_2022[c(-unwantedCols)]
df_03_2022 <- df_03_2022[c(-unwantedCols)]
df_04_2022 <- df_04_2022[c(-unwantedCols)]
df_05_2022 <- df_05_2022[c(-unwantedCols)]
df_06_2022 <- df_06_2022[c(-unwantedCols)]
df_07_2022 <- df_07_2022[c(-unwantedCols)]
df_08_2022 <- df_08_2022[c(-unwantedCols)]
df_09_2022 <- df_09_2022[c(-unwantedCols)]
df_10_2022 <- df_10_2022[c(-unwantedCols)]
df_11_2022 <- df_11_2022[c(-unwantedCols)]
df_12_2022 <- df_12_2022[c(-unwantedCols)]


#check dataframe after cols deleted 
str(df_01_2022)

colnames(df_01_2022)

#combineData
df_total <- rbind(df_01_2022, df_02_2022, df_03_2022, df_04_2022, df_05_2022,
                  df_06_2022, df_07_2022, df_08_2022, df_09_2022, df_10_2022,
                  df_11_2022, df_12_2022)

#clean up space
remove(df_01_2022, df_02_2022, df_03_2022, df_04_2022, df_05_2022,
       df_06_2022, df_07_2022, df_08_2022, df_09_2022, df_10_2022,
       df_11_2022, df_12_2022)

#clean up data and remove blank rows
nrow(df_total)

#remove blank rows
df_total <- na.omit(df_total)

#remove duplicate rows
df_total <- df_total[!duplicated(df_total),]


#Add columns for data analysis
df_total$started_at_datetime <- ymd_hms(df_total$started_at)
df_total$startDate <- format(df_total$started_at_datetime,'%Y-%m-%d')
df_total$startTime <- format(as.POSIXct(df_total$started_at_datetime), format = "%H:%M:%S")
df_total$startHour <- format(as.POSIXct(df_total$started_at_datetime), format = "%H")
df_total$ended_at_datetime <- ymd_hms(df_total$ended_at)
df_total$endDate <- format(df_total$ended_at_datetime,'%Y-%m-%d')
df_total$endTime <- format(as.POSIXct(df_total$ended_at_datetime), format = "%H:%M:%S")
df_total$endHour <- format(as.POSIXct(df_total$ended_at_datetime), format = "%H")
df_total$DurationMins <- difftime(df_total$ended_at_datetime, df_total$started_at_datetime, units = "mins")
df_total$RideDuration <- as.double(df_total$DurationMins)
df_total$RideDurationHours <- as.double(df_total$RideDuration/60)
df_total$month <- format(df_total$started_at_datetime,'%m')
df_total$weekDay <- wday(df_total$started_at_datetime)
df_total$week <- week(df_total$startDate)
df_total$monthName <- month.abb[df_total$month]

#all look ok?
str(df_total)

#factor character cols
df_total$rideable_type <- factor(df_total$rideable_type)
df_total$member_casual <- factor(df_total$member_casual)
df_total$monthName <- factor(df_total$monthName)

#change relevant columns to numeric
df_total$startHour <- as.numeric(df_total$startHour)
df_total$endHour <- as.numeric(df_total$endHour)
df_total$month <- as.numeric(df_total$month)

#remove rows with negative ride duration
df_total<- df_total[!(df_total$RideDuration <= 0),]

#longest rides - remove all longer than 24 hours
df_longestRides <-  df_total %>% arrange(desc(RideDurationHours), week) %>% filter(RideDurationHours > 24)
df_total <- df_total[!(df_total$RideDurationHours %in% df_longestRides$RideDurationHours),]

#total rows left
totalrides = nrow(df_total)




#Rides by different people groups
grp_memcas =df_total %>% 
    group_by(member_casual) %>% 
    summarise(total =n()) 

s <- ggplot(data=grp_memcas, aes(x=member_casual, y=total, fill=member_casual))
s<- s +geom_col(position='dodge', stat='identity') + 
    geom_text(aes(label = formatC(total, format="f", big.mark=",", digits=0)), vjust = 1.5, colour = "white")
s<- s + xlab("Member Type") + #x axis title
    ylab("Total Number of Rides") + #y axis title
    ggtitle("Member and Casual Riders") #plot title
s<- s + theme(axis.title.x = element_text(color="DarkGreen", size=20),
              axis.title.y = element_text(color="DarkGreen", size=20),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              plot.title = element_text(size=20, color="DarkGreen", family = "Courier"),
              legend.position = "none")
s


#bikes the people chose
grp_mem_cas_ride_type <- df_total %>% 
    group_by(member_casual, rideable_type) %>% 
    summarise(total =n()) 

p<- ggplot(data=grp_mem_cas_ride_type, aes(fill=rideable_type, y= total, x=member_casual))
p <- p + geom_bar(position='dodge', stat='identity') + 
    scale_fill_discrete(name = "Type", labels=c("Classic Bike", "Docked Bike", "Electric Bike"))
p <- p + xlab("Member Type") + #x axis title
    ylab("Total Number of Rides") + #y axis title
    ggtitle("Member and Casual Rides by Bike Type") #plot title

p <- p + theme(axis.title.x = element_text(color="DarkGreen", size=20),
               axis.title.y = element_text(color="DarkGreen", size=20),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=20, color="DarkGreen", family = "Courier"))
p



#group by time
grp_monthname <- df_total %>% 
    group_by(monthName) %>% 
    summarise(total =n())

s <- ggplot(data=grp_monthname, aes(x=monthName, y=total, fill="Red"))
s <- s+ geom_bar(stat="identity")+ scale_x_discrete(limits = month.abb)
s<- s + xlab("Month") + #x axis title
    ylab("Total Number of Rides") + #y axis title
    ggtitle("Ride Distribution by Month") #plot title

s<- s + theme(axis.title.x = element_text(color="DarkGreen", size=20),
              axis.title.y = element_text(color="DarkGreen", size=20),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              plot.title = element_text(size=20, color="DarkGreen", family = "Courier"),
              legend.position = "none")
s

#group by rider type and time
grp_monthName_member_casual <- df_total %>% 
    group_by(monthName,member_casual) %>% 
    summarise(total =n())

p<- ggplot(data=grp_monthName_member_casual, aes(fill=member_casual, y= total, x=monthName))
p <- p + geom_col(position='dodge', stat='identity') + scale_x_discrete(limits =month.abb) + 
    scale_fill_discrete(name = "Type")
p <- p + xlab("Month") + #x axis title
    ylab("Total Number of Rides") + #y axis title
    ggtitle("Member and Casual Rides by Month") #plot title

p <- p + theme(axis.title.x = element_text(color="DarkGreen", size=20),
               axis.title.y = element_text(color="DarkGreen", size=20),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=20, color="DarkGreen", family = "Courier"))
p


grp_week_member_casual <- df_total %>% 
    group_by(week,member_casual) %>% 
    summarise(total =n())

s<- ggplot(data=grp_week_member_casual, aes(fill=member_casual, x=week, y=total))
s+geom_bar(position='dodge', stat='identity')

p<- ggplot(data=grp_week_member_casual, aes(fill=member_casual, y= total, x=week))
p <- p + geom_bar(position='dodge', stat='identity')
p <- p + xlab("Week") + #x axis title
    ylab("Total Number of Rides") + #y axis title
    ggtitle("Member and Casual Rides by Week") #plot title

p <- p + theme(axis.title.x = element_text(color="DarkGreen", size=20),
               axis.title.y = element_text(color="DarkGreen", size=20),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=20, color="DarkGreen", family = "Courier"))
p



#group by day and hour
grp_weekDay_hour_member_casual <-
    df_total %>% 
    group_by(weekDay,startHour,member_casual) %>% 
    summarise(total =n())
#merge day and hour
grp_weekDay_hour_member_casual <- transform(grp_weekDay_hour_member_casual, weekDay_Hour=paste(weekDay,startHour,sep=""))

#split out weekdays and weekends into separate dfs
grp_weekend_hour_member_casual <- grp_weekDay_hour_member_casual[grp_weekDay_hour_member_casual$weekDay==1 | grp_weekDay_hour_member_casual$weekDay==7,]
rownames(grp_weekend_hour_member_casual) <- 1:nrow(grp_weekend_hour_member_casual)
grp_weekend_hour_member_casual$roworder = as.numeric(rownames(grp_weekend_hour_member_casual))

#add time details to weekend df
timestamps <- seq(as.POSIXct("2023-04-08 00:00:00"),as.POSIXct("2023-04-08 23:00:00"),by = "hour")
formatted_timestamps <- format(timestamps, format = "%H:%M")
formatted_timestamps <- sort(rep(formatted_timestamps, each = 2))

formatted_timestamps <- c(formatted_timestamps,formatted_timestamps)
grp_weekend_hour_member_casual$timestamps <- formatted_timestamps

s<- ggplot(data=grp_weekend_hour_member_casual, aes(fill=member_casual, x=roworder, y=total))
s<- s + geom_bar(position='dodge', stat='identity') + scale_fill_discrete(name = "Member Type")
s <- s + scale_x_continuous(breaks = c(25, 50, 75), labels = c("12:00", "00:00", "12:00"))
s<- s + geom_vline(xintercept = c(25, 50, 75), linetype = "solid", color = "black")
s <- s + xlab("Saturday / Sunday") + #x axis title
    ylab("Total Number of Rides") + #y axis title
    ggtitle("Total Member and Casual Rides at the Weekend") #plot title

s <- s + theme(axis.title.x = element_text(color="DarkGreen", size=20),
               axis.title.y = element_text(color="DarkGreen", size=20),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=20, color="DarkGreen", family = "Courier"))
s


#now do the same for weekdays
grp_weekday_hour_member_casual <- grp_weekDay_hour_member_casual[grp_weekDay_hour_member_casual$weekDay!=1 & grp_weekDay_hour_member_casual$weekDay!=7,]
rownames(grp_weekday_hour_member_casual) <- 1:nrow(grp_weekday_hour_member_casual)
grp_weekday_hour_member_casual$roworder = as.numeric(rownames(grp_weekday_hour_member_casual))

s<- ggplot(data=grp_weekday_hour_member_casual, aes(fill=member_casual, x=roworder, y=total))
s<- s + geom_bar(position='dodge', stat='identity') + scale_fill_discrete(name = "Member Type")
s <- s + scale_x_continuous(breaks = c(24, 73, 121, 169, 217 ), labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
s<- s + geom_vline(xintercept = c(49, 97,145, 193), linetype = "solid", color = "black", size = 1)
s <- s + xlab("Week Day") + #x axis title
    ylab("Total Number of Rides") + #y axis title
    ggtitle("Total Member and Casual Rides During Week") #plot title

s <- s + theme(axis.title.x = element_text(color="DarkGreen", size=20),
               axis.title.y = element_text(color="DarkGreen", size=20),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=20, color="DarkGreen", family = "Courier"))
s




#mean duration by day
meanDuration_weekDay_member_casual <- df_total %>% group_by(weekDay,member_casual) %>%
    summarise(m=mean(RideDuration))


p<- ggplot(data=meanDuration_weekDay_member_casual, aes(fill=member_casual, y= m, x=weekDay))
p <- p + geom_col(position='dodge', stat='identity') + scale_fill_discrete(name = "Type")
p <- p + scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
p <- p + xlab("WeekDay") + #x axis title
    ylab("Average Ride Duration / Mins") + #y axis title
    ggtitle("Member and Casual Rides Average Ride Duration by Weekday") #plot title

p <- p + theme(axis.title.x = element_text(color="DarkGreen", size=15),
               axis.title.y = element_text(color="DarkGreen", size=15),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=15, color="DarkGreen", family = "Courier"))
p


#mean duration by month

meanduration_monthName_member_casual <- df_total %>% group_by(monthName,member_casual) %>%
    summarise(m=mean(RideDuration))

p<- ggplot(data=meanduration_monthName_member_casual, aes(fill=member_casual, y= m, x=monthName))
p <- p + geom_col(position='dodge', stat='identity') + scale_x_discrete(limits =month.abb) + 
    scale_fill_discrete(name = "Type")
p <- p + xlab("Month") + #x axis title
    ylab("Average Duration Minutes") + #y axis title
    ggtitle("Member and Casual Member Average Ride Duration by Month") #plot title

p <- p + theme(axis.title.x = element_text(color="DarkGreen", size=20),
               axis.title.y = element_text(color="DarkGreen", size=20),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=15, color="DarkGreen", family = "Courier"))
p



#mean duration by day and hour, same process as above

meanduration_weekDay_startHour_member_casual <- df_total %>% group_by(weekDay,startHour,member_casual) %>%
    summarise(m=mean(RideDuration))

meanduration_weekend_startHour_member_casual <- meanduration_weekDay_startHour_member_casual[meanduration_weekDay_startHour_member_casual$weekDay==1 | meanduration_weekDay_startHour_member_casual$weekDay==7,]
rownames(meanduration_weekend_startHour_member_casual) <- 1:nrow(meanduration_weekend_startHour_member_casual)
meanduration_weekend_startHour_member_casual$roworder = as.numeric(rownames(meanduration_weekend_startHour_member_casual))

s<- ggplot(data=meanduration_weekend_startHour_member_casual, aes(color=member_casual, x=roworder, y=m), size=5)
s<- s + geom_line()+ scale_fill_discrete(name = "Member Type") + scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
s <- s + scale_x_continuous(breaks = c(25, 50, 75), labels = c("12:00", "00:00", "12:00"))
s<- s + geom_vline(xintercept = c(25, 50, 75), linetype = "solid", color = "black")
s <- s + xlab("Saturday / Sunday") + #x axis title
    ylab("Average Ride Duration / Minutes") + #y axis title
    ggtitle("Average Ride Length for Member and Casual Rides at the Weekend") #plot title

s <- s + theme(axis.title.x = element_text(color="DarkGreen", size=12),
               axis.title.y = element_text(color="DarkGreen", size=12),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=15, color="DarkGreen", family = "Courier"))

s


meanduration_weekDay_startHour_member_casual <- meanduration_weekDay_startHour_member_casual[meanduration_weekDay_startHour_member_casual$weekDay!=1 & meanduration_weekDay_startHour_member_casual$weekDay!=7,]
rownames(meanduration_weekDay_startHour_member_casual) <- 1:nrow(meanduration_weekDay_startHour_member_casual)
meanduration_weekDay_startHour_member_casual$roworder = as.numeric(rownames(meanduration_weekDay_startHour_member_casual))


s<- ggplot(data=meanduration_weekDay_startHour_member_casual, aes(color=member_casual, x=roworder, y=m), size=5)
s<- s + geom_line()+ scale_fill_discrete(name = "Member Type") + scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
s <- s + scale_x_continuous(breaks = c(24, 73, 121, 169, 217 ), labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
s<- s + geom_vline(xintercept = c(49, 97,145, 193), linetype = "solid", color = "black", size = 0.5)
s <- s + xlab("Week Day") + #x axis title
    ylab("Average Ride Duration / Minutes") + #y axis title
    ggtitle("Average Ride Duration Member and Casual Rides During Week") #plot title

s <- s + theme(axis.title.x = element_text(color="DarkGreen", size=12),
               axis.title.y = element_text(color="DarkGreen", size=12),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=15, color="DarkGreen", family = "Courier"))

s



