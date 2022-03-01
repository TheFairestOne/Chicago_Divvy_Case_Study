### Divvy_Exercise_Full_Year_Analysis ###

# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Load required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(readxl) #importing excel files

setwd("/Users/fair/Documents/Google Data Analytics Professional Certificate/8 Capstone Project/Case_Study1_BikeShare/Excel Data/2021") 

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets 
jan_2021 <- read_excel("202101-divvy-tripdata.xlsx")
feb_2021 <- read_excel("202102-divvy-tripdata.xlsx")
march_2021 <- read_excel("202103-divvy-tripdata.xlsx")
april_2021 <- read_excel("202104-divvy-tripdata.xlsx")
may_2021 <- read_excel("202105-divvy-tripdata.xlsx")
june_2021 <- read_excel("202106-divvy-tripdata.xlsx")
july_2021 <- read_excel("202107-divvy-tripdata.xlsx")
aug_2021 <- read_excel("202108-divvy-tripdata.xlsx")
sept_2021 <- read_excel("202109-divvy-tripdata.xlsx")
oct_2021 <- read_excel("202110-divvy-tripdata.xlsx")
nov_2021 <- read_excel("202111-divvy-tripdata.xlsx")
dec_2021 <- read_excel("202112-divvy-tripdata.xlsx")
jan_2022 <- read_excel("202201-divvy-tripdata.xlsx")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files before appending them together
colnames(jan_2021)
colnames(feb_2021)
colnames(march_2021)
colnames(april_2021)
colnames(may_2021)
colnames(june_2021)
colnames(july_2021)
colnames(aug_2021)
colnames(sept_2021)
colnames(oct_2021)
colnames(nov_2021)
colnames(dec_2021)
colnames(jan_2022)


# Combine all of the months into one large data frame
all_trips <- bind_rows(jan_2021, feb_2021, march_2021, april_2021,may_2021,june_2021,july_2021,aug_2021,sept_2021,oct_2021,nov_2021,dec_2021,jan_2022)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.
tail(all_trips) #See the last 6 rows of data frame.
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. 

# Add columns that list the date, month, day, hour, and year of each ride so data 
# can be aggregated for each month, day, hour, or year 
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$hour <- format(all_trips$started_at,"%H")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspecting the structure of the columns
str(all_trips)

# Convert "ride_length" from to numeric to run calculations on the data
#typeof(all_trips$ride_length)
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
str(all_trips)

summary(all_trips$ride_length)
# Removing bad data
# The dataframe includes entries where ride_length was negative that need to
# be removed, as well as rides lasting longer than a day, which are likely
# errors due to missing bikes or locking/unlocking issues
# Creating a new dataframe (v2) to remove data
all_trips_v2 <- all_trips[!(all_trips$ride_length<=0 | 
                              all_trips$ride_length>86400),]
#checking to make sure new numbers make sense
mean(all_trips$ride_length)
mean(all_trips_v2$ride_length)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See how members vs casual rides vary by month
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$month, FUN = mean)

# Setting order for the days of week when sorting
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Visualization for number of rides by day of week by rider type
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + ggtitle("Average Number of Rides by Day of the Week")
  
# Save each plot 
ggsave("number_rides_by_weekday_by_ridertype.png")

# Visualization for average duration by day of week by rider type
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + ggtitle("Average Ride Duration by Day of the Week")

ggsave("duration_by_weekday_by_ridertype.png")

# Visualization for ride duration by month by rider type
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) + 
  geom_col(position = "dodge") + ggtitle("Average Ride Duration by Ride Start Month")

ggsave("duration_by_month_by_ridertype.png")

# Visual for number of rides by month by rider type
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month,y=number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") + ggtitle("Average Number of Rides by Ride Start Month")

ggsave("number_rides_by_month_by_ridertype.png")

# Visual for number of rides by rider by start hour by rider type
all_trips_v2 %>% 
  group_by(member_casual, hour) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hour) %>% 
  ggplot(aes(x=hour,y=number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") + ggtitle("Average Number of Rides by Ride Start Hour")

ggsave("number_rides_by_hour_by_ridertype.png")

# Visual for duration of rides by rider by start hour by rider type
all_trips_v2 %>% 
  group_by(member_casual, hour) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hour) %>% 
  ggplot(aes(x=hour,y=average_duration, fill = member_casual)) + 
  geom_col(position = "dodge") + ggtitle("Average Ride Duration by Ride Start Hour")

ggsave('duration_by_hour_ridertype.png')

# Visual for type of bike preference by rider type
all_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + ggtitle("Average Number of Rides of Different Bike Types")

ggsave('bike_preference_by_membership.png')

# Visual for type of bike preference by ride length by rider type
all_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x = rideable_type, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +ggtitle("Average Ride Duration of Different Bike Types")

ggsave('bike_duration_by_membership.png')

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or another presentation software
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week + all_trips_v2$month + all_trips_v2$hour, FUN = mean)
colnames(counts)
str(counts)
write.csv(counts, file = '~/Documents/counts.csv')
#writes it alllll to a csv (fingers crossed, please dont break)
write.csv(all_trips_v2, file = '~/Documents/all_trips_vs.csv')
#This file was too large for Tableau, so making a smaller one.
tableau_data <- all_trips_v2 %>%  
  select(-c(ended_at, end_station_name, end_station_id, end_lat, end_lng))

write.csv(tableau_data, file='~/Documents/tableau_data.csv')


