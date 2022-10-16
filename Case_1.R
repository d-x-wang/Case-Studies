install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(dplyr)
library(tidyr)
library(skimr)
library(lubridate)
library(janitor)

# Extract Past 12 Months Files and Consolidate to DB
files = list.files(path = "C://Users//dlwan//Documents//Google_Certificate//Case1//Data")
setwd("C://Users//dlwan//Documents//Google_Certificate//Case1//Data")
for (x in 1:length(files)) {
  trip_x= read_csv(files[x])
  if (x==1){
    trip = trip_x
  } 
  # Check if column structure is the same in each file
  else if (compare_df_cols_same(trip,trip_x,bind_method="rbind") == TRUE){
  trip = rbind(trip,trip_x)
  }
}

# Add column for travel time (seconds)
trip=trip %>% 
  mutate(ride_length = ended_at - started_at)

# Add column for day of week (Sunday = 1, Saturday = 7)
trip=trip %>% 
  mutate(day_of_week = wday(started_at))

# Clean Data
# Include data with trip duration greater than 0 sec and valid day of week
# Remove rows with NA in any cell
trip2022 = trip %>% 
  filter(ride_length>0) %>% 
  filter(day_of_week!="") %>% 
  na.omit()

# Export to csv:
write.csv(trip2022,r"(C:\Users\dlwan\Documents\Google_Certificate\Case1\Trip2022.csv)",row.names=FALSE)

# Statistics: average ride length, 
trip2022 %>% 
  group_by(member_casual) %>% 
  summarize(mean_ride_time = mean(ride_length),mode_day = names(which.max(table(day_of_week))),mode_ride = names(which.max(table(rideable_type))),number = length(day_of_week))

# Histogram of day of week traveled
## Casual users mainly use bikes on weekend
## Members mainly use bikes on weekdays
ggplot(data=trip2022)+
  geom_bar(mapping=aes(x=day_of_week,fill=member_casual), position = position_dodge2(width=0.4)) +
  ggtitle("Daily Riders") + 
  xlab("Day") +
  ylab("Number of Riders")

## Sort based on rideable type
## docked bike is used only by casual
## classic bike is favored by members
## electric bike is favored by casual
ggplot(data=trip2022)+
  geom_bar(mapping=aes(x=day_of_week,fill=rideable_type), position = position_dodge2(width=0.5))+
  facet_wrap(~member_casual)+
  ggtitle("Mode of Transportation") + 
  xlab("Day") +
  ylab("Number of Riders")


# Histogram of ride times
## Majority of rides occur less than 10 minutes
ggplot(data=trip2022)+
  geom_histogram(mapping=aes(x=(as.numeric(ride_length)),fill=member_casual), position = position_dodge(width=225),breaks = seq(0,5000,250))+
  ggtitle("Ride Time") + 
  xlab("Time") +
  ylab("Number of Riders")

ggplot(data=trip2022)+
  geom_histogram(mapping=aes(x=(as.numeric(ride_length)),fill=member_casual), position = position_dodge(width=225),breaks = seq(0,5000,250)) +
  facet_wrap(~rideable_type)

trip2022 %>% 
  group_by(member_casual) %>%
  summarize(mean_ride_time = mean(ride_length),mode_day = names(which.max(table(day_of_week))),
            mode_ride = names(which.max(table(rideable_type))),cnt = n(),
            percent_riders = (cnt/nrow(.)*100))

trip2022 %>% 
  group_by(member_casual) %>% 
  mutate(ride_length_min = minute(seconds_to_period(ride_length))) %>% 
  summarize(mean_ride_time_min = mean(ride_length_min),mode_day = names(which.max(table(day_of_week))),mode_ride = names(which.max(table(rideable_type))),
            number_of_riders = n(),percent_riders = (n()/nrow(.)*100))


trip2022 %>% 
  group_by(rideable_type) %>% 
  mutate(ride_length_min = minute(seconds_to_period(ride_length))) %>% 
  summarize(mean_ride_time_min = mean(ride_length_min),
            number_of_riders = n(),percent_riders = round((n()/nrow(.)*100),1))


## Histogram number of rides per month
## Most popular months are summer time.
ggplot(data=trip2022)+
  geom_bar(mapping=aes(x=factor(months(started_at,abbreviate=TRUE),levels = month.abb),fill=member_casual), position = position_dodge(width=0.9)) +
  ggtitle("Monthly Riders") + 
  xlab("Month") +
  ylab("Number of Riders")

## Histogram top locations
# Top 10 starting locations
trip2022_loc = trip2022 %>% 
  group_by(start_station_name) %>% 
  summarize(num_ppl = length(start_station_name)) %>% 
  filter(rank(desc(num_ppl))<=10) %>% 
  arrange(desc(num_ppl))

trip2022_loc_all = trip2022 %>% 
  group_by(start_station_name) %>% 
  summarize(num_ppl = length(start_station_name)) %>% 
  arrange(desc(num_ppl))

ggplot(data=trip2022_loc) +
  geom_bar(mapping=aes(x=(reorder(start_station_name,num_ppl)), y =num_ppl),stat = "identity") +
  ggtitle("Location") + 
  xlab("Location") +
  ylab("Number of Riders") +
  coord_flip()

## Histogram top locations
# Top 10 ending locations
trip2022_loc_end = trip2022 %>% 
  group_by(end_station_name) %>% 
  summarize(num_ppl = length(end_station_name)) %>% 
  filter(rank(desc(num_ppl))<=10) %>% 
  arrange(desc(num_ppl))

trip2022_loc__end_all = trip2022 %>% 
  group_by(end_station_name) %>% 
  summarize(num_ppl = length(end_station_name)) %>% 
  arrange(desc(num_ppl))

ggplot(data=trip2022_loc_end) +
  geom_bar(mapping=aes(x=(reorder(end_station_name,num_ppl)), y =num_ppl),stat = "identity") +
  ggtitle("Location") + 
  xlab("Location") +
  ylab("Number of Riders") +
  coord_flip()

# Ride Times based on Ride Type
trip2022 %>% 
  mutate(ride_length_min = minute(seconds_to_period(ride_length))) %>% 
  ggplot()+
    geom_bar(mapping=aes(x=member_casual,y = ride_length_min, fill=rideable_type), position = position_dodge2(width=0.5),
             stat = "summary", fun.y = "mean") +
    ggtitle("Mode of Transportation") + 
    xlab("Member") +
    ylab("Average Ride Length, min")

# Number of Rides Hourly
ggplot(data=trip2022)+
  geom_bar(mapping=aes(x=hour(started_at), fill=member_casual), position = position_dodge2(width=0.5))+
  ggtitle("Hourly Ride") + 
  xlab("Hour") +
  ylab("Number of Rides, min")