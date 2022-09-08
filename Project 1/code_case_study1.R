##Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(data.table)

##Load data
bike_data <- list.files(pattern='tripdata',full.names=TRUE,recursive=TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

glimpse(bike_data)

##Clean data
bike_data <- bike_data %>% 
  select(-c(start_lat:end_lng))

colnames(bike_data)

bike_data <- bike_data[!duplicated(bike_data$ride_id), ]

nrow(bike_data)

#Number of test rides
nrow(subset(bike_data, tolower(start_station_name) %like% "test"))

#Removing test rides
bike_data <- bike_data[!(tolower(bike_data$start_station_name) %like% "test"), ]
nrow(bike_data)

##Data Manipulation

#Create new column ride_length
bike_data <- bike_data %>% 
  mutate(ride_length = as.numeric(difftime(bike_data$ended_at, bike_data$started_at, units = "secs")))

glimpse(bike_data)

#Check for negative values
nrow(bike_data[bike_data$ride_length < 0, ])

#Remove negative values
bike_data <- bike_data %>% 
  filter(ride_length >= 0)
glimpse(bike_data)

bike_data_clean <- bike_data %>% 
  mutate(month_yr = strftime(started_at, "%b-%Y"),
         weekday_name = weekdays(as.Date(started_at)),
         start_hr = strftime(started_at, "%H"))

glimpse(bike_data_clean)

#Save results
bike_data_clean %>% 
  write.csv('cyclists_clean.csv')


##Analyze
cyclists <- read_csv('cyclists_clean.csv', col_names = TRUE)

cyclists %>%
  group_by(member_casual) %>%
  summarise(count = n(),
            percentage = (count/nrow(cyclists))*100)
#Bar chart
cyclists %>% 
  ggplot(aes(x = member_casual, fill = member_casual))+
  geom_bar()+
  labs(title = "Members vs. Casuals", subtitle = "Counting number of members and casuals")+
  scale_y_continuous(labels = comma)

#Pie chart
df_pie <- cyclists %>%
  group_by(member_casual) %>%
  summarise(count = n())

df_pie %>%
  ggplot(aes(x = "", y = count, fill = member_casual))+
  geom_bar(stat = "identity", width = 1, color = "white")+
  coord_polar("y", start = 0)+
  labs(title = "Members vs. Casuals", subtitle = "Percentage of Customer Satus")+
  theme_void()

#Month table
x1 <- cyclists %>%
  group_by(member_casual, month_yr) %>%
  summarise(number_of_rides = n(), avg_ride_time = mean(ride_length)) %>%
  arrange(member_casual, desc(number_of_rides))

print(tibble(x1), n = 24)

#Arrange month year 
cyclists$month_yr <- ordered(cyclists$month_yr, 
                             levels = c("Aug-2021","Sep-2021","Oct-2021","Nov-2021","Dec-2021",
                                        "Jan-2022","Feb-2022","Mar-2022","Apr-2022","May-2022",
                                        "Jun-2022","Jul-2022"))

#Visual for Number of Rides per month
cyclists %>%
  group_by(member_casual) %>%
  ggplot(aes(x = month_yr, fill = member_casual))+
  geom_bar(position = "dodge")+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Rides per Month")+
  scale_y_continuous(labels = comma)
#Visual for Average Duration of Bike rides per month
cyclists %>% 
  group_by(member_casual, month_yr) %>%
  summarise(avg_ride_secs = mean(ride_length)) %>%
  ggplot(aes(x = month_yr, y = avg_ride_secs, fill = member_casual))+
  geom_col(width = 0.75, position = "dodge")+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Average Bike Ride Duration for Every Month")+
  scale_y_continuous(labels = comma)

#Week day
cyclists$weekday_name <- ordered(cyclists$weekday_name,
                                 levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                                            'Thursday', 'Friday', 'Saturday'))

cyclists %>% 
  group_by(member_casual, weekday_name) %>% 
  ggplot(aes(x = weekday_name, fill = member_casual))+
  geom_bar(position = 'dodge')+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Rides Per Weekday", subtitle = "Grouped by Customer Status")+
  scale_y_continuous(labels = comma)

#Find the mode
getmode <- function(x) {
  u = unique(x)
  u[which.max(tabulate(match(x,u)))]
}

aggregate(cyclists$weekday_name ~ cyclists$member_casual, FUN = getmode)

#Weekday Table
cyclists %>%
  group_by(member_casual, weekday_name) %>%
  summarise(number_of_rides = n(), avg_ride_time_secs = mean(ride_length)) %>%
  arrange(member_casual, desc(number_of_rides))

#Average ride length per day of the week
cyclists %>%
  group_by(member_casual, weekday_name) %>%
  summarise(avg_bike_ride = mean(ride_length)) %>%
  ggplot(aes(x = weekday_name, y = avg_bike_ride, fill = member_casual))+
  geom_col(width = 0.75, position = "dodge")+
  labs(title = "Average Bike Ride Duration Over Week")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_y_continuous(labels = comma)

#Hour Visual
cyclists %>% 
  group_by(member_casual,start_hr) %>%
  summarise(bike_rides = n()) %>%
  ggplot(aes(x = start_hr, y = bike_rides, color = member_casual, group = member_casual))+
  geom_line()+
  labs(title = "Bike Rides Every Hour")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_y_continuous(labels = comma)

#Type of Bike table 
cyclists %>%
  group_by(member_casual, rideable_type) %>%
  summarise(bike_type_count = n()) %>%
  arrange(member_casual, desc(bike_type_count))

#Stacked bar chart of bike type
cyclists %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual))+
  geom_col()+
  labs(title = "Ride Type Between Customers")+
  scale_y_continuous(labels = comma)

#Summary of Stastics of Customers and Bike duration
summary(cyclists$ride_length)

aggregate(cyclists$ride_length ~ cyclists$member_casual, FUN = mean)

aggregate(cyclists$ride_length ~ cyclists$member_casual, FUN = median)

aggregate(cyclists$ride_length ~ cyclists$member_casual, FUN = max)

aggregate(cyclists$ride_length ~ cyclists$member_casual, FUN = min)


