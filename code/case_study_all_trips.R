library(readr)
library(conflicted)
conflict_prefer("filler", "dyplyr")
conflict_prefer("lag", "dyplyr")

#Step 1 Collect DATA

q1_19 <- read_csv("Divvy_Trips_2019_Q1.csv")

q1_20 <- read_csv("Divvy_Trips_2020_Q1.csv")

# Step 2 Wrangle Data and Combine into a Single file

colnames(q1_19)
colnames(q1_20)

#Rename columns to match both tables
(q1_19 <- rename(q1_19
            ,ride_id = trip_id
            ,rideable_type = bikeid
            ,started_at = start_time
            ,ended_at = end_time
            ,start_station_name = from_station_name
            ,start_station_id = from_station_id
            ,end_station_name = to_station_name
            ,end_station_id = to_station_id
            ,member_casual = usertype
            ))
#Inspect dataframes for incongruencies
str(q1_19)
str(q1_20)
#rideable_type and ride_id need to mutate

q1_19 <- mutate(q1_19, ride_id = as.character(ride_id)
                ,rideable_type = as.character(rideable_type))

#Stack Individual quarters dataframes into a single large one
all_trips <- bind_rows(q1_19, q1_20)#,q3_19)#, q4_19, q1_20)

#Remove lat, long, birthyear, and gender fields as this data was dropped in 2020
all_trips<- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))

#Step 3 Clean up and add data to prepare for analysis
#check if new table was created and column names
colnames(all_trips)

nrow(all_trips) #how many rows in new data frame?
dim(all_trips) #Dimensions of the new dataframe(how many rows and columns?
head(all_trips) #Visualize the first 6 rows of data frame includes all colums
str(all_trips) #See list of columns and data types(number, character,..)
summary(all_trips) #Statistical summary of Data, mainly for numbers

#In member_casual column replace Subscriber with member and Customer with casual
#N.B. "Level" is a special property of a column that is retained even if a subset
#does not contain any values from a specific level
#Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)

#Reassign to the desired values, going with current 2020 labels
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual
                    ,"Subscriber" = "member"
                    ,"Customer" = "casual"))

#Check if observations were reassigned
table(all_trips$member_casual)

#Add columns that list the date, month, day, and year of each ride
#this will allow us to aggregate ride data for each month, day or year
#before this we could only aggregate the ride level
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#Add a ride length calculation to all trips (seconds)
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

#Inspect the structure of the columns
str(all_trips)

#Convert ride length from factor to numeric to run calculations
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#Remove BAD data
#The dataframe inclues a few hundred entries when bikes were taken out of docks
#and checked for quality by DIVVY, or  ride length is negative
#We will create a new version of the dataframe since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#Descriptive analysis on ride length(all figures in seconds)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#days of week are not sorted 
all_trips_v2$day_of_week<- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednsday", "Thursday", "Friday", "Saturday"))

#new average with days sorted
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #Creates weekday field using
  #wday()
  group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n()#calculates the number of rides and avg duration
            ,average_duration = mean(ride_length)) %>% #calculates avg duration
      arrange(member_casual, weekday)
  
#Lets visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y =number_of_rides, fill = member_casual))+
  geom_col(position = "dodge")

#create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y =average_duration, fill = member_casual))+
  geom_col(position = "dodge")

#Step 5 export summary in CSV
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')