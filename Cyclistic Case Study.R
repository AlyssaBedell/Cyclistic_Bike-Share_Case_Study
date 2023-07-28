#PACKAGES-----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(scales)
library(rmarkdown)

#DATA---------------------------------------------------------------------
Cyclistic_Jan <- read.csv("202201-divvy-tripdata.csv")
Cyclistic_Feb <- read.csv("202202-divvy-tripdata.csv")
Cyclistic_Mar <- read.csv("202203-divvy-tripdata.csv")
Cyclistic_Apr <- read.csv("202204-divvy-tripdata.csv")
Cyclistic_May <- read.csv("202205-divvy-tripdata.csv")
Cyclistic_Jun <- read.csv("202206-divvy-tripdata.csv")
Cyclistic_Jul <- read.csv("202207-divvy-tripdata.csv")
Cyclistic_Aug <- read.csv("202208-divvy-tripdata.csv")
Cyclistic_Sep <- read.csv("202209-divvy-publictripdata.csv")
Cyclistic_Oct <- read.csv("202210-divvy-tripdata.csv")
Cyclistic_Nov <- read.csv("202211-divvy-tripdata.csv")
Cyclistic_Dec <- read.csv("202212-divvy-tripdata.csv")

# Merging the data
Cyclistic <- rbind(
  Cyclistic_Jan, Cyclistic_Feb, Cyclistic_Mar, Cyclistic_Apr, Cyclistic_May, Cyclistic_Jun, Cyclistic_Jul, Cyclistic_Aug, Cyclistic_Sep, Cyclistic_Oct, Cyclistic_Nov, Cyclistic_Dec
)

#Take a look at the data now
head(Cyclistic)

#NA values
#Number of NA values
sum(is.na(Cyclistic)) 

#Number of NA values per column
colSums(is.na(Cyclistic))

#Get rid of NAs
Cyclistic <- drop_na(Cyclistic)

#Blank cells
Cyclistic %>% 
  filter(start_station_name=="") %>% 
  count()

Cyclistic %>% 
  filter(start_station_id=="") %>% 
  count()

Cyclistic %>% 
  filter(end_station_name=="") %>% 
  count()

Cyclistic %>% 
  filter(end_station_id=="") %>% 
  count()

#Delete these rows
Cyclistic <- subset(Cyclistic, start_station_name != "")
Cyclistic <- subset(Cyclistic, end_station_name != "") 

#Add ride_length
Cyclistic <- Cyclistic %>% mutate(started_at = ymd_hms(started_at))
Cyclistic <- Cyclistic %>% mutate(ended_at = ymd_hms(ended_at))
Cyclistic <- mutate(Cyclistic, ride_length = ended_at - started_at)

summary(Cyclistic$ride_length)
Cyclistic %>%
  summarise(min(ride_length), max(ride_length))#Negative time and extremely long?


Cyclistic %>%
  arrange(ride_length)#End time before start time,super short rides(less than 60 secs), long ride length(several days)


#Bad data
#Trips with end time before start time
bad_data <- subset(Cyclistic, ride_length <= 0)
View(bad_data)
Cyclistic <- subset(Cyclistic, ride_length > 0) #Deletes these trips from our dataset
#Trips with duration of less than 60 seconds and where starting station is the same as the ending station
bad_data <- subset(Cyclistic, ride_length < 60 & start_station_id == end_station_id)
glimpse(bad_data)
sum(duplicated(Cyclistic$ride_id))#Check ride_id for duplicates
Cyclistic <- filter(Cyclistic, !(ride_id %in% bad_data$ride_id))#Deletes these trips from our dataset
#Trips with super long ride_length (over 24 hours)
bad_data <- subset(Cyclistic, ride_length > 86400)
glimpse(bad_data)
Cyclistic <- subset(Cyclistic, ride_length <= 86400)#Deletes these trips from our dataset

#Seperate day of the week
Cyclistic <- mutate(Cyclistic,
                   date_of_trip = date(started_at), day = day(date_of_trip),
                   month = month(date_of_trip), year = year(date_of_trip)
)
Cyclistic <- mutate(Cyclistic, weekday = weekdays(date_of_trip))

#Change casual_member to user_type to simplify
Cyclistic <- rename(Cyclistic, user_type = member_casual)

#Make a column just for the date 
Cyclistic$date <- as.Date(Cyclistic$started_at)


#Make a column for the month
Cyclistic$month_name <- format(as.Date(Cyclistic$date), "%b")

#ANALYSIS-----------------------------------------------------------------

#Number of rides
Cyclistic %>%
  group_by(user_type) %>%
  count()
Cyclistic %>%
  group_by(user_type, weekday) %>%
  count() #More rides are done by members


#Average ride-length
mean(Cyclistic$ride_length)

#Average ride_length for casual riders and members
aggregate(Cyclistic$ride_length ~ Cyclistic$user_type, FUN = median)


#Max ride_length
max(Cyclistic$ride_length)


#Sort weekedays
Cyclistic$weekday <- ordered(Cyclistic$weekday, levels = c('Sunday', 'Monday', 'Tuesday',
                                                         'Wednesday', 'Thursday', 'Friday',
                                                         'Saturday'))

#Mode day_of_week
#Make a mode function
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
#Use new mode function
find_mode(Cyclistic$weekday)

#Number of casuals and members in a table per week day?
table(Cyclistic$user_type,Cyclistic$weekday)

#Number of casuals and members in a table per month?
table(Cyclistic$user_type,Cyclistic$month_name)

#User percentage?
user_percentage <- Cyclistic %>%
  group_by(user_type) %>%
  summarize(total = n()) %>%
  mutate(overall_total = sum(total)) %>%
  group_by(user_type) %>%
  summarize((percent_total = total/overall_total))

user_percentage


#How riders use the bikes per day of the week
ggplot(data = Cyclistic) +
  geom_bar(mapping = aes(x = weekday, fill = user_type), position = "dodge") +
  labs(
    title = "Member vs Casual: Number of Rides Per Day of The Week",
    subtitle = "January 2022 to December 2022",
    x = "Day of The Week",
    y = "Number of Rides",
    fill = "User Type"
  ) +
  scale_y_continuous(labels = comma)

#Number of rides per user type
Cyclistic %>%
  group_by(user_type, year, month) %>%
  count()
ggplot(data = Cyclistic) +
  geom_bar(mapping = aes(x = month, fill = user_type), position = "dodge") +
  labs(
    title = "Member vs Casual: Number of Rides Per Month",
    subtitle = "January 2022 to December 2022",
    x = "Month", 
    y = "Number of Rides", 
    fill = "User Type"
  )+ 
  scale_x_discrete(limits = 1:12) +
  scale_y_continuous(labels = comma)

#Compare ride_length
Cyclistic %>%
  group_by(user_type) %>%
  summarise(mean_ride_length = mean(ride_length))
Cyclistic %>%
  group_by(user_type, weekday) %>%
  summarise(mean(ride_length))#The average ride length of casual riders is higher

Cyclistic %>%
  group_by(user_type, weekday) %>%
  summarise(mean_ride_length = mean(ride_length), .groups = "drop") %>%
  ggplot(aes(x = weekday, y = as.numeric(mean_ride_length), fill = user_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    title = "Member vs Casual: Average Ride Length Per Day of The Week",
    subtitle = "January 2022 to December 2022",
    x = "Day of The Week",
    y = "Average Ride Length (Seconds)",
    fill = "User Type"
  ) 

#Average ride_length of the user types per month
Cyclistic %>%
  group_by(user_type, year, month) %>%
  summarise(mean(ride_length))
Cyclistic %>%
  group_by(user_type, year, month) %>%
  summarise(mean_ride_length = mean(ride_length), .groups = "drop") %>%
  ggplot(aes(x = month, y = as.numeric(mean_ride_length), fill = user_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    title = "Member vs Casual: Average Ride Length Per Month",
    subtitle = "January 2022 to December 2022",
    x = "Month",
    y = "Average Ride Length (Seconds)",
    fill = "User Type"
  ) + scale_x_discrete(limits = 1:12)

#Hourly time of day
ggplot(data = Cyclistic) + 
  geom_bar(mapping = aes(x= hour(started_at), fill = user_type), position = 'dodge') +
  labs(title = "Member vs Casual: Hourly Bike Use",
       subtitle = "January 2022 to December 2022",
       x = "Time of The Day",
       y = "Number of Rides",
       fill = "User Type"
       )+ scale_y_continuous(labels = comma)

#Hourly time of day for each day separately
ggplot(data = Cyclistic) + 
  geom_bar(mapping = aes(x= hour(started_at), fill = user_type), position = 'dodge') +
  facet_wrap(~weekday) +
  labs(title = "Member vs Casual: Hourly Bike Use Per Weekday",
       subtitle = "January 2022 to December 2022",
       x = "Time of The Day",
       y = "Number of Rides",
       fill = "User Type"
       )+ scale_y_continuous(labels = comma)

#rideable_type
Cyclistic %>% 
  group_by(rideable_type, user_type) %>% 
  count()

ggplot(data = Cyclistic) +
  geom_bar(mapping = aes(x = rideable_type, fill = user_type), position = 'dodge') +
  labs(title = "Member vs Casual: Type of Bike",
       subtitle = "January 2022 to December 2022",
       x = "Type of Bike",
       y = "Number of Rides",
       fill = "User Type"
       ) +
  scale_y_continuous(labels = comma)

ggplot(data = Cyclistic) +
  geom_bar(mapping = aes(x = rideable_type, fill = user_type), position = "dodge") +
  facet_wrap(~weekday) + 
  labs(title = "Member vs Casual: Type of Bike Per Day",
       subtitle = "January 2022 to December 2022",
       x = "Type of Bike",
       y = "Number of Rides",
       fill = "User Type"
       ) +
  scale_y_continuous(labels = comma)

#Most common stations
#Casual
Cyclistic %>% # Checking the start station
  filter(user_type == 'casual') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5) 

Cyclistic %>% # Checking the end station
  filter(user_type == 'casual') %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)

#Differences for weekday or not?
Cyclistic %>% 
  filter(user_type == 'casual') %>%
  filter(weekday == "Saturday" | weekday == "Sunday") %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)

Cyclistic %>% 
  filter(user_type == 'casual') %>%
  filter(weekday != "Saturday" | weekday != "Sunday") %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)

Cyclistic %>% 
  filter(user_type == 'casual') %>%
  filter(weekday == "Saturday" | weekday == "Sunday") %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)

Cyclistic %>% 
  filter(user_type == 'casual') %>%
  filter(weekday != "Saturday" | weekday != "Sunday") %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)


#Members
Cyclistic %>% 
  filter(user_type == 'member') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)

Cyclistic %>% 
  filter(user_type == 'member') %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)

#Differences for weekday or not?
Cyclistic %>% 
  filter(user_type == 'member') %>%
  filter(weekday == "Saturday" | weekday == "Sunday") %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)

Cyclistic %>% 
  filter(user_type == 'member') %>%
  filter(weekday != "Saturday" | weekday != "Sunday") %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)

Cyclistic %>% 
  filter(user_type == 'member') %>%
  filter(weekday == "Saturday" | weekday == "Sunday") %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)

Cyclistic %>% 
  filter(user_type == 'member') %>%
  filter(weekday != "Saturday" | weekday != "Sunday") %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(-number_of_rides) %>% 
  print(n=5)

#The most used stations are different for member/casual riders#




