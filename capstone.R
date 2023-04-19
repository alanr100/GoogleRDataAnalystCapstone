getwd()

library(tidyverse) #general covers lots
library(lubridate) #date manipulation
library(dplyr) # statistical help
library(ggplot2)

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

str(df_01_2022)

colnames(df_01_2022)

#combineData
#df_total <- df_01_2022
df_total <- rbind(df_01_2022, df_02_2022, df_03_2022, df_04_2022, df_05_2022,
                  df_06_2022, df_07_2022, df_08_2022, df_09_2022, df_10_2022,
                  df_11_2022, df_12_2022)

#clean up space
remove(df_01_2022, df_02_2022, df_03_2022, df_04_2022, df_05_2022,
       df_06_2022, df_07_2022, df_08_2022, df_09_2022, df_10_2022,
       df_11_2022, df_12_2022)

#clean up data and remove blank rows
nrow(df_total)
df_total <- na.omit(df_total)





#count duplicate rows
#nrow(df_total[duplicated(df_total), ])
#df_total[duplicated(df_total), ]
#remove duplicate rows

nrow(df_total)
df_total <- df_total[!duplicated(df_total),]
nrow(df_total)


#df_total$started_at
#convert started_at to datetime as currently string
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

str(df_total)


#dd to list for easier manipulation
#list_df <- list(df_total, df_02_2022, df_03_2022, df_04_2022, df_05_2022, df_06_2022,
#                df_07_2022,df_08_2022, df_09_2022, df_10_2022,df_11_2022, df_12_2022  )
##cOME BACK HERE


#remove first column from each table as riderid is not required
#also remove start_station_name, start_station_id   end_station_name   end_station_id   ,start_lat, start_long, end_lat, end_lng    

#list_df <- lapply(list_df, "[",,-c(1,6,8,9,10,11,12))

df_total$rideable_type <- factor(df_total$rideable_type)
df_total$member_casual <- factor(df_total$member_casual)
df_total$monthName <- factor(df_total$monthName)



#rideable_type and member_casual     should be  factors

#change columns to numeric
df_total$startHour <- as.numeric(df_total$startHour)
df_total$endHour <- as.numeric(df_total$endHour)
df_total$month <- as.numeric(df_total$month)

str(df_total)

colnames(df_total)


nrow(df_total)
#count rows na, 0 or negative and remove
df_total<- df_total[!(df_total$RideDuration <= 0),]
nrow(df_total)

#longest rides - remove all longer than 
df_longestRides <-  df_total %>% arrange(desc(RideDurationHours), week) %>% filter(RideDurationHours > 24)
df_total <- df_total[!(df_total$RideDurationHours %in% df_longestRides$RideDurationHours),]

#count duplicate rows
#nrow(df_total[duplicated(df_total), ])
#df_total[duplicated(df_total), ]
#remove duplicate rows

#nrow(df_total)
#df_total <- df_total[!duplicated(df_total),]


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




#docked bikes onlt used by casual members. Classic bike most popular however difference
#for casual members is  21%

(grp_mem_cas_ride_type[grp_mem_cas_ride_type$member_casual=="casual" &  grp_mem_cas_ride_type$rideable_type=="classic_bike",3] - grp_mem_cas_ride_type[grp_mem_cas_ride_type$member_casual=="casual" & grp_mem_cas_ride_type$rideable_type=="electric_bike",3])/grp_mem_cas_ride_type[grp_mem_cas_ride_type$member_casual=="casual" & grp_mem_cas_ride_type$rideable_type=="classic_bike",3]


    
#bike grouping
grp_ride_type <- df_total %>% 
    group_by(rideable_type) %>% 
    summarise(total =n()) 

#group by time
grp_month <- df_total %>% 
    group_by(month) %>% 
    summarise(total =n())
s<- ggplot(data=grp_month, aes(x=month, y=total))
s+ geom_bar(stat="identity")

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



grp_startDate <- df_total %>% 
    group_by(startDate) %>% 
    summarise(total =n())

q <- ggplot(data=grp_startDate, aes(x=startDate, y = total))
q+ geom_point()

grp_week <- df_total %>% 
    group_by(week) %>% 
    summarise(total =n())

s<- ggplot(data=grp_week, aes(x=week, y=total))
s+ geom_bar(stat="identity")



grp_weekday <- df_total %>% 
    group_by(weekDay) %>% 
    summarise(total =n())

s<- ggplot(data=grp_weekday, aes(x=weekDay, y=total))
s+ geom_bar(stat="identity")



grp_hour <- df_total %>% 
    group_by(startHour) %>% 
    summarise(total =n())

s<- ggplot(data=grp_hour, aes(x=startHour, y=total))
s+ geom_bar(stat="identity")





#group by rider type and time
grp_month_member_casual <- df_total %>% 
    group_by(month,member_casual) %>% 
    summarise(total =n())

grp_monthName_member_casual <- df_total %>% 
    group_by(monthName,member_casual) %>% 
    summarise(total =n())

p<- ggplot(data=grp_monthName_member_casual, aes(fill=member_casual, y= total, x=monthName))
p <- p + geom_col(position='dodge', stat='identity') + scale_x_discrete(limits =month.abb) + 
    scale_fill_discrete(name = "Type")
#p <- p + coord_flip()
p <- p + xlab("Month") + #x axis title
    ylab("Total Number of Rides") + #y axis title
    ggtitle("Member and Casual Rides by Month") #plot title

p <- p + theme(axis.title.x = element_text(color="DarkGreen", size=20),
               axis.title.y = element_text(color="DarkGreen", size=20),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=20, color="DarkGreen", family = "Courier"))
p




grp_startDate_member_casual <- df_total %>% 
    group_by(startDate,member_casual) %>% 
    summarise(total =n())


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



grp_weekday_member_casual <- df_total %>% 
    group_by(weekDay,member_casual) %>% 
    summarise(total =n())

s<- ggplot(data=grp_weekday_member_casual, aes(fill=member_casual, x=weekDay, y=total))
s+geom_bar(position='dodge', stat='identity')


grp_hour_member_casual <-
    df_total %>% 
    group_by(startHour,member_casual) %>% 
    summarise(total =n())

s<- ggplot(data=grp_hour_member_casual, aes(fill=member_casual, x=startHour, y=total))
s+geom_bar(position='dodge', stat='identity')

grp_weekDay_hour_member_casual <-
    df_total %>% 
    group_by(weekDay,startHour,member_casual) %>% 
    summarise(total =n())

grp_weekDay_hour_member_casual <- transform(grp_weekDay_hour_member_casual, weekDay_Hour=paste(weekDay,startHour,sep=""))

grp_weekend_hour_member_casual <- grp_weekDay_hour_member_casual[grp_weekDay_hour_member_casual$weekDay==1 | grp_weekDay_hour_member_casual$weekDay==7,]
rownames(grp_weekend_hour_member_casual) <- 1:nrow(grp_weekend_hour_member_casual)
grp_weekend_hour_member_casual$roworder = as.numeric(rownames(grp_weekend_hour_member_casual))

#
timestamps <- seq(as.POSIXct("2023-04-08 00:00:00"),as.POSIXct("2023-04-08 23:00:00"),by = "hour")
formatted_timestamps <- format(timestamps, format = "%H:%M")
formatted_timestamps
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



#group by rider type, bike, time
grp_month_member_casual_bike_type <- df_total %>% 
    group_by(month,member_casual,rideable_type) %>% 
    summarise(total =n())

grp_startDate_member_casual_bike_type <- df_total %>% 
    group_by(startDate,member_casual,rideable_type) %>% 
    summarise(total =n())

grp_week_member_casual_bike_type <- df_total %>% 
    group_by(week,member_casual,rideable_type) %>% 
    summarise(total =n())

grp_weekday_member_casual_bike_type <- df_total %>% 
    group_by(weekDay,member_casual,rideable_type) %>% 
    summarise(total =n())

grp_hour_member_casual_bike_type <- df_total %>% 
    group_by(startHour,member_casual,rideable_type) %>% 
    summarise(total =n())

#######################################################################

#######################################################################
#average ride length by grouping

meanDuration = mean(df_total$RideDuration)

meanDuration_biketype <- df_total %>% group_by(rideable_type) %>%
    summarise(m=mean(RideDuration))

meanDuration_member_casual <- df_total %>% group_by(member_casual) %>%
    summarise(m=mean(RideDuration))

meanDuration_hour <- df_total %>% group_by(startHour) %>%
    summarise(m=mean(RideDuration))

meanDuration_startDate <- df_total %>% group_by(startDate) %>%
    summarise(m=mean(RideDuration))

meanDuration_month <- df_total %>% group_by(month) %>%
    summarise(m=mean(RideDuration))

meanDuration_weekDay <- df_total %>% group_by(weekDay) %>%
    summarise(m=mean(RideDuration))

meanDuration_week <- df_total %>% group_by(week) %>%
    summarise(m=mean(RideDuration))

s = ggplot(data=meanDuration_week, aes(x=week, y=m))
s+geom_line()

#################################################################
# averge ride time by member type

meanduration_member_casual <- df_total %>% group_by(member_casual) %>%
    summarise(m=mean(RideDuration))

meanduration_hour_member_casual <- df_total %>% group_by(startHour,member_casual) %>%
    summarise(m=mean(RideDuration))

meanduration_date_member_casual <- df_total %>% group_by(startDate,member_casual) %>%
    summarise(m=mean(RideDuration))
s<-ggplot(data=meanduration_date_member_casual, aes(x=startDate, y=m, color=member_casual))
s + geom_point()         

meanDuration_weekDay_member_casual <- df_total %>% group_by(weekDay,member_casual) %>%
    summarise(m=mean(RideDuration))


p<- ggplot(data=meanDuration_weekDay_member_casual, aes(fill=member_casual, y= m, x=weekDay))
p <- p + geom_col(position='dodge', stat='identity') + scale_fill_discrete(name = "Type")
#p <- p + coord_flip()
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


meanduration_month_member_casual <- df_total %>% group_by(month,member_casual) %>%
    summarise(m=mean(RideDuration))
s<-ggplot(data=meanduration_month_member_casual, aes(x=month, y=m, color=member_casual))
s + geom_line()           

meanduration_monthName_member_casual <- df_total %>% group_by(monthName,member_casual) %>%
    summarise(m=mean(RideDuration))

p<- ggplot(data=meanduration_monthName_member_casual, aes(fill=member_casual, y= m, x=monthName))
p <- p + geom_col(position='dodge', stat='identity') + scale_x_discrete(limits =month.abb) + 
    scale_fill_discrete(name = "Type")
#p <- p + coord_flip()
p <- p + xlab("Month") + #x axis title
    ylab("Average Duration Minutes") + #y axis title
    ggtitle("Member and Casual Member Average Ride Duration by Month") #plot title

p <- p + theme(axis.title.x = element_text(color="DarkGreen", size=20),
               axis.title.y = element_text(color="DarkGreen", size=20),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               plot.title = element_text(size=15, color="DarkGreen", family = "Courier"))
p





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
#s<- s + geom_col(position='dodge', stat='identity') + scale_fill_discrete(name = "Member Type")
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



