
install.packages("tidyverse")
install.packages("vroom")
library(tidyverse)
library(vroom)
library(lubridate)

#setwd("C:/Users/Alex/Documents/Data Analysis/Case Studies/CS1 Data/cyclistic-bike-data")

#append data, remove geospatial columns, rename columns
all_trips2021 <- vroom(list.files(pattern = "*.csv", recursive = TRUE))
all_trips2021 <- select(all_trips2021, -c(start_lat, start_lng, end_lat, end_lng))

all_trips2021 <- rename(all_trips2021
                        ,trip_id = ride_id
                        ,bikeid = rideable_type
                        ,start_time = started_at
                        ,end_time = ended_at
                        ,from_station_name = start_station_name
                        ,from_station_id = start_station_id
                        ,to_station_name = end_station_name
                        ,to_station_id = end_station_id
                        ,usertype = member_casual)

#create column "tripduration" and set to a numeric class in v2
all_trips2021v2 <-  mutate(all_trips2021, tripduration = end_time - start_time) %>% 
  filter(tripduration > 0)
all_trips2021v2$tripduration <- as.numeric(all_trips2021v2$tripduration)

#new columns for dates
all_trips2021v2$date <- as.Date(all_trips2021v2$start_time) #The default format is yyyy-mm-dd
all_trips2021v2$month <- format(as.Date(all_trips2021v2$date), "%m")
all_trips2021v2$day <- format(as.Date(all_trips2021v2$date), "%d")
all_trips2021v2$year <- format(as.Date(all_trips2021v2$date), "%Y")
all_trips2021v2$day_of_week <- format(as.Date(all_trips2021v2$date), "%A")


all_trips2021v2$day_of_week <- ordered(all_trips2021v2$day_of_week
                                       ,levels=c("Sunday",  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips2021v2$tripduration/60 ~ all_trips2021v2$usertype + all_trips2021v2$day_of_week, FUN = mean)

#summary table
trips_summaries <- all_trips2021v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(usertype, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(tripduration)/60) %>% # calculates the average duration
  arrange(usertype, weekday)

# Average trip duration graph
weekdayTripLength <- trips_summaries %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = 'dodge') +
  labs(title = "Average Trip Duration by Weekday") +
  xlab("Day of the Week") +
  ylab("Average Trip Duration (minutes)")

# Number of trips per weekday graph
options(scipen = 999) #no scientific notation for # of rides label
weekdayRides <- trips_summaries %>% 
  ggplot(aes(x = weekday, y = number_of_rides/1000, fill = usertype)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Weekday (thousands)") +
  xlab("Day of the Week") +
  ylab("Total rides")
