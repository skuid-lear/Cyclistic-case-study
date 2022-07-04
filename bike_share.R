install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
library(tidyverse)
library(lubridate)
library(ggplot2)
setwd()
getwd()
m5_2021 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202105-divvy-tripdata.csv")
m6_2021 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202106-divvy-tripdata.csv")
m7_2021 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202107-divvy-tripdata.csv")
m8_2021 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202108-divvy-tripdata.csv")
m9_2021 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202109-divvy-tripdata.csv")
m10_2021 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202110-divvy-tripdata.csv")
m11_2021 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202111-divvy-tripdata.csv")
m12_2021 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202112-divvy-tripdata.csv")
m1_2022 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202201-divvy-tripdata.csv")
m2_2022 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202202-divvy-tripdata.csv")
m3_2022 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202203-divvy-tripdata.csv")
m4_2022 <- read.csv("/Users/beatricealford/Desktop/Coursera_Capstone1_Csv_Files/202204-divvy-tripdata.csv")
colnames(m5_2021)
colnames(m6_2021)
colnames(m7_2021)
colnames(m8_2021)
colnames(m9_2021)
colnames(m10_2021)
colnames(m11_2021)
colnames(m12_2021)
colnames(m1_2022)
colnames(m2_2022)
colnames(m3_2022)
colnames(m4_2022)

str(m5_2021)
str(m6_2021)
str(m7_2021)
str(m8_2021)
str(m9_2021)
str(m10_2021)
str(m11_2021)
str(m12_2021)
str(m1_2022)
str(m2_2022)
str(m3_2022)
str(m4_2022)

all_trips <- rbind(m5_2021, m6_2021, m7_2021, m8_2021, m9_2021, m10_2021, m11_2021, m12_2021, m1_2022, m2_2022, m3_2022, m4_2022)
all_trips[,-c(9,10,11,12)]

colnames(m5_2021)
colnames(m6_2021)
colnames(m7_2021)
colnames(m8_2021)
colnames(m9_2021)
colnames(m10_2021)
colnames(m11_2021)
colnames(m12_2021)
colnames(m1_2022)
colnames(m2_2022)
colnames(m3_2022)
colnames(m4_2022)

colnames(all_trips)
all_trips[, -c(9,10.11,12)]
str(all_trips)
install.packages("dplyr")
library(dplyr)
select(all_trips, -c(9,10,11,12))
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
all_trips <-all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)
View(all_trips)
install.packages("datasets")
library(datasets)
install.packages("dplyr")
library(dplyr)
library(tidyverse)
all_trips$date <- as.Date(all_trips$started_at)
install.packages("Rcpp")
library(Rcpp)
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
colnames(all_trips)
str(all_trips)
View(all_trips)
head(all_trips)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
colnames(all_trips_v2)
all_trips_v2 <- all_trips
View(all_trips_v2)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
mean(all_trips_v2$ride_length) 
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length)
summary(all_trips_v2$ride_length)
summary(all_trips_v2)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)    
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/Coursera_Capstone1_Csv_Files/avg_ride_length.csv')


