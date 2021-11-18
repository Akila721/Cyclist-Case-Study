## Importing the libraries to be used
library(ggplot2)
library(dplyr)
library(plyr)
library(anytime)
library(lubridate)
library(scales)

### Adding the .csv files to file list
file_list <- list.files(path = ".", pattern = "*-divvy-tripdata.csv")

### Extracting the csv file list to "data" object
data <- ldply(.data = file_list, .fun=read.csv)

str(data)

### Changing "started_at" and "ended_at" data types into datetime
data$started_at <- anytime(data$started_at)
data$ended_at <- anytime(data$ended_at)

str(data)

data <- data %>%
  mutate(duration = ended_at - started_at)

### Filtering negative trip duration
negative_durations <- filter(data, duration < 0)
negative_durations

filtered_data <- filter(data, duration > 0)

str(filtered_data)

filtered_data <- filtered_data %>%
  mutate(day_of_the_week = wday(filtered_data$started_at, week_start = 1))

filtered_data <- filtered_data %>%
  mutate(weekday = ifelse(day_of_the_week >= 6, "weekend", "weekday"))

filtered_data <- filtered_data %>%
  mutate(year_month = format(as.POSIXct(filtered_data$started_at), format = "%Y%m"))


## Plots
### Plot with member Type and the started date of the week
ggplot(data=filtered_data, aes(fill=member_casual, x=day_of_the_week)) +
  geom_bar(position="dodge", stat="count") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(axis.text.x = element_text()) +
  labs(y = "Trips", x = "Day of Week", fill = "Member Type")

### Plot with member Type and the Month
ggplot(data=filtered_data, aes(fill=member_casual, x=year_month)) +
  geom_bar(position="dodge", stat="count") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(axis.text.x = element_text()) +
  labs(y = "Trips", x = "Months", fill = "Member Type")

### Plot with member Type and Bike Type
ggplot(data=filtered_data, aes(fill=rideable_type, x=day_of_the_week)) +
  geom_bar(position="stack", stat="count") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "# of Trips", x = "Week Days") +
  scale_fill_discrete(name="Bike Type", labels = c("Docked","Electric")) +
  facet_wrap(~member_casual)

### Plot with duration with customer
ggplot(data=filtered_data, aes(duration)) +
  geom_histogram(binwidth = 900) +
  xlim(0, 3600) +
  scale_y_continuous(labels = scales::label_number_si()) +
  facet_wrap(~member_casual)

### Plot with duration with Bike
ggplot(data=filtered_data, aes(duration)) +
  geom_histogram(binwidth = 900) +
  xlim(0, 3600) +
  scale_y_continuous(labels = scales::label_number_si()) +
  facet_wrap(~rideable_type)

### Plot with duration with Day of the week
ggplot(data=filtered_data, aes(duration)) +
  geom_histogram(binwidth = 900) +
  xlim(0, 3600) +
  scale_y_continuous(labels = scales::label_number_si()) +
  facet_wrap(~day_of_the_week)

## End of Plots

str(filtered_data)

## Top Start Stations for Casual members
start_stations <- filtered_data %>%
  filter(start_station_name != "") %>% 
  filter(member_casual == "casual") %>%
  group_by(start_station_name) %>%
  tally(sort = TRUE) %>%
  ungroup() %>%
  arrange(desc(n))
head(start_stations, 10)

## Top Start Stations for Non-Casual members
start_stations <- filtered_data %>%
  filter(start_station_name != "") %>% 
  filter(member_casual == "member") %>%
  group_by(start_station_name) %>%
  tally(sort = TRUE) %>%
  ungroup() %>%
  arrange(desc(n))
head(start_stations, 10)

## Top Start Stations for Docked bikes
start_stations <- filtered_data %>%
  filter(start_station_name != "") %>% 
  filter(rideable_type == "docked_bike") %>%
  group_by(start_station_name) %>%
  tally(sort = TRUE) %>%
  ungroup() %>%
  arrange(desc(n))
head(start_stations, 10)

## Top Start Stations for Electric bikes
start_stations <- filtered_data %>%
  filter(start_station_name != "") %>% 
  filter(rideable_type == "electric") %>%
  group_by(start_station_name) %>%
  tally(sort = TRUE) %>%
  ungroup() %>%
  arrange(desc(n))
head(start_stations, 10)


