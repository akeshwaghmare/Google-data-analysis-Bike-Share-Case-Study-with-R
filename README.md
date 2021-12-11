# Google-data-analysis-Bike-Share-Case-Study-with-R
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
tripdata_202012 <- read_csv("202012-divvy-tripdata.csv")
tripdata_202101 <- read_csv("202101-divvy-tripdata.csv")
tripdata_202102 <- read_csv("202102-divvy-tripdata.csv")
tripdata_202103 <- read_csv("202103-divvy-tripdata.csv")
tripdata_202104 <- read_csv("202104-divvy-tripdata.csv")
tripdata_202105 <- read_csv("202105-divvy-tripdata.csv")
tripdata_202106 <- read_csv("202106-divvy-tripdata.csv")
tripdata_202107 <- read_csv("202107-divvy-tripdata.csv")
tripdata_202108 <- read_csv("202108-divvy-tripdata.csv")
tripdata_202109 <- read_csv("202109-divvy-tripdata.csv")
tripdata_202110 <- read_csv("202110-divvy-tripdata.csv")
tripdata_202111 <- read_csv("202111-divvy-tripdata.csv")
tripdata <- rbind(tripdata_202012,
                  tripdata_202101,
                  tripdata_202102,
                  tripdata_202103,
                  tripdata_202104,
                  tripdata_202105,
                  tripdata_202106,
                  tripdata_202107,
                  tripdata_202108,
                  tripdata_202109,
                  tripdata_202110,
                  tripdata_202111)
glimpse(tripdata)

# Remove rows with missing values
colSums(is.na(tripdata))
tripdata_cleaned <- tripdata[complete.cases(tripdata), ]

# data with started_at greater than ended_at will be removed
tripdata_cleaned <- tripdata_cleaned %>% 
  filter(tripdata_cleaned$started_at < tripdata_cleaned$ended_at)

# create new column `ride_length`
tripdata_cleaned$ride_length <- tripdata_cleaned$ended_at - tripdata_cleaned$started_at
tripdata_cleaned$ride_length <- hms::hms(seconds_to_period(tripdata_cleaned$ride_length))

# create new column `day_of_week`
library(lubridate)
tripdata_cleaned$day_of_week <- wday(tripdata_cleaned$started_at, label = FALSE)

# mean of ride_length
tripdata_cleaned %>% 
  summarize(mean(ride_length))

# max ride_length
tripdata_cleaned %>% 
  summarize(max(ride_length))

# min ride_length
tripdata_cleaned %>% 
  summarize(min(ride_length))

# mode of day_of_week
library(DescTools)
Mode(tripdata_cleaned$day_of_week)

# average ride_length for members and casual riders
tripdata_cleaned %>% 
  group_by(member_casual) %>% 
  summarize(mean(ride_length))

# average ride_length for users by day_of_week
tripdata_cleaned %>% 
  group_by(day_of_week) %>% 
  summarize(mean(ride_length))

# number of rides for users by day_of_week
tripdata_cleaned %>% 
  group_by(ride_id, day_of_week) %>% 
  summarize(number_of_rides=n())

#  average ride time by each day for members vs casual users
aggregate(tripdata_cleaned$ride_length ~ tripdata_cleaned$member_casual + tripdata_cleaned$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
tripdata_cleaned %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

# visualize number of rides by rider type
tripdata_cleaned %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Number of Rides by Days and Rider Type",
       subtitle = "Members versus Casual Users") +
  ylab("Number of Rides") +
  xlab("Day of Week")

# visualization for average duration
tripdata_cleaned %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Average Duration of Rides by Days and Rider Type",
       subtitle = "Members versus Casual Users") +
  ylab("Average Duration of Rides") +
  xlab("Day of Week")

# average ride_length by type and day of week
counts <- aggregate(tripdata_cleaned$ride_length ~ tripdata_cleaned$member_casual +
                      tripdata_cleaned$day_of_week, FUN = mean)

write.csv(counts, file = 'avg_ride-length.csv')

# average ride_length and type and month
tripdata_cleaned$month <- month(tripdata_cleaned$started_at, label = TRUE)

rides <- aggregate(tripdata_cleaned$ride_length ~ tripdata_cleaned$member_casual +
                     tripdata_cleaned$month,FUN = mean)

write.csv(rides, file = 'avg_ride_length_by_month.csv')
