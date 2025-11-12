library(tidyverse)
library(lubridate)
library(hms)
library(ggplot2)

q1_2019 <- read.csv("~/Case_Study_Cyclistic/Divvy_Trips_2019_Q1.csv")
q1_2020 <- read.csv("~/Case_Study_Cyclistic/Divvy_Trips_2020_Q1.csv")

# Column names
colnames(q1_2019)
colnames(q1_2020)

# First few rows
head(q1_2019)
head(q1_2020)

# Data structure
str(q1_2019)
str(q1_2020)

# Change the values in the columns user_type and member_casual so they match
q1_2019 <- q1_2019 %>%
  mutate(usertype = recode(usertype, "Subscriber" = "member", "Customer" = "casual"))

# Rename the columns so they match in both datasets
q1_2020 <- q1_2020 %>%
  rename(usertype = member_casual, start_time = started_at, end_time = ended_at)

q1_2019 <- q1_2019 %>%
  rename(start_station_id = from_station_id, start_station_name = from_station_name , end_station_id = to_station_id, end_station_name = to_station_name)

# Select only interesting columns for our analysis by removing columns that we don't need
q1_2019 <- q1_2019 %>%
  select(-bikeid, -gender, -birthyear, -trip_id, -tripduration)

q1_2020 <- q1_2020 %>%
  select(-ride_id, -rideable_type, -start_lat, -start_lng, -end_lat, -end_lng)

# Combine the two datasets
data_2019_2020 <- bind_rows(q1_2019, q1_2020)

# Create a new column to get the ride length in HH:MM:SS

# First, we need to convert from character to datetime type 
data_2019_2020 <- data_2019_2020 %>%
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time))

# Then, we format as HH:MM:SS
data_2019_2020 <- data_2019_2020 %>%
  mutate(ride_length = as_hms(end_time - start_time))

# Create a new column to get the day of week
data_2019_2020 <- data_2019_2020 %>%
  mutate(day_of_week = wday(start_time))

# Clean data (remove missing values, only positive ride_length and less than 24 hours ride length, not ride tests ("HQ QR"))
data_2019_2020 <- data_2019_2020 %>%
  drop_na()

data_2019_2020 <- data_2019_2020 %>%
  filter(ride_length > 0 & ride_length < 86400 & start_station_name != "HQ QR")   # 24 hours is 86400 seconds

# Get all the statistics about the ride_length of the dataset grouping it by usertype !!
analysis_summary <- data_2019_2020 %>%
  group_by(usertype) %>%
  summarise(
    total_rides = n(),
    avg_ride_length = mean(ride_length, na.rm = TRUE),
    median_ride_length = median(ride_length, na.rm = TRUE),
    max_ride_length = max(ride_length, na.rm = TRUE),
    min_ride_length = min(ride_length, na.rm = TRUE)
  )

# Average ride length by day
day_avg_summary <- data_2019_2020 %>%
  group_by(usertype, day_of_week) %>%
  summarise(avg_ride_length = mean(ride_length, na.rm = TRUE)) %>%
  pivot_wider(names_from = day_of_week, values_from = avg_ride_length)

# Number of rides by day
day_count_summary <- data_2019_2020 %>%
  group_by(usertype, day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  pivot_wider(names_from = day_of_week, values_from = number_of_rides)
  
data_2019_2020 <- mutate(data_2019_2020, day_name = wday(start_time, label = TRUE, abbr = FALSE), ride_length_seconds = as.numeric(ride_length))
write_csv(data_2019_2020, "cyclistic_data_cleaned2.csv")
getwd()


