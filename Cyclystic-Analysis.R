

# load libraries required for processing and analysis 

library(tidyverse) # used for the data wrangling and data manipulation 
library(janitor) # used for data cleaning 
library(DataExplorer)
library(lubridate) # used to easily convert data types of date and time formats
library(skimr)

# Clean the files environment in the R work space ( best practice to always start fresh)

rm(list=ls())

# set the current work directory to be the 2021 directory 

setwd("~/Google_DataAnalytics_Course/Capstone_Project/Track_1/CaseStudy_1/Cyclistic/Working_Dir/CSV/2021")

# create Data frames from all the files in the directory 

df1 <- read.csv("202101-divvy-tripdata.csv")
df2 <- read.csv("202102-divvy-tripdata.csv")
df3 <- read.csv("202103-divvy-tripdata.csv")
df4 <- read.csv("202104-divvy-tripdata.csv")
df5 <- read.csv("202105-divvy-tripdata.csv")
df6 <- read.csv("202107-divvy-tripdata.csv")
df7 <- read.csv("202107-divvy-tripdata.csv")
df8 <- read.csv("202108-divvy-tripdata.csv")


#### Examine the data to determine problems


# examine each file to ensure consistent col names and investigate the data 

str(df1)
str(df2)
str(df3)
str(df4)
str(df5)
str(df6)
str(df7)
str(df8)


# checking columns names to ensure the ability to combine without issues
colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)
colnames(df5)
colnames(df6)
colnames(df7)
colnames(df8)


#Looking at the Structure of each dataset we can see that they can be easily combined into one master file for easier cleanup and exploring


# Create one Variable Data frame with all the df's combined with rbind function (this requires the data frame structure to be matching)

bike_rides_2021 <- rbind(df1,df2,df3,df4,df5,df6,df7,df8)

# View a Summary of the data to see the data structure and investigate any issues with the data

skim_without_charts(bike_rides_2021)



#As we can see from the data skim function , there is some cleaning needed :
  
  #-   Some missing values in the end_lat and end_long data , start_station_name , end_station_name)

  #-   data types need to be fixed ( dates : started_at , ended_at )

#### Cleaning the Data



# drop na and null values from the Dataset 

bike_rides_2021 <- drop_na(bike_rides_2021)

# Change data types of the array data 

bike_rides_2021$started_at <- lubridate::as_datetime(bike_rides_2021$started_at)
bike_rides_2021$ended_at <- lubridate::as_datetime(bike_rides_2021$ended_at)

# Calculate ride length and add new column 

bike_rides_2021$ride_length <- difftime(bike_rides_2021$ended_at,bike_rides_2021$started_at, units = c("auto"))

# add the day of the week to the data 
bike_rides_2021$day_of_week <- lubridate::wday(bike_rides_2021$started_at , label = TRUE )

#clean the ride_length to remove negative values 
bike_rides_2021 <- bike_rides_2021 %>%  filter(bike_rides_2021$ride_length > 0 )



# View a Summary of the data after cleaning to check 

skim_without_charts(bike_rides_2021)


# Analyze Step

In this step we will work to perform some calculations on grouped data as well as some basic trend and relationship insights

# create ride length variable by converting original from factor to numeric 

bike_rides_2021$ride_length_num <- as.numeric(bike_rides_2021$ride_length)

#focus the analysis on the Member vs Casual data so will aggreagte based on the two conditons of memeber and casual for later export. 

mean_rides <- aggregate(bike_rides_2021$ride_length_num ~ bike_rides_2021$member_casual , FUN = mean)

max_rides <- aggregate(bike_rides_2021$ride_length_num ~ bike_rides_2021$member_casual , FUN = max)

mean_rides_day_of_week <- aggregate(bike_rides_2021$ride_length_num ~ bike_rides_2021$member_casual + bike_rides_2021$day_of_week , FUN = mean)

#aggregate the data based on the station location 

loc_rides_start <- aggregate(bike_rides_2021$ride_length ~ bike_rides_2021$start_station_name+bike_rides_2021$start_lat + bike_rides_2021$start_lng +bike_rides_2021$start_station_id + bike_rides_2021$ride_id + bike_rides_2021$member_casual , bike_rides_2021  , mean )

#export the data for external analysis and plotting using Tableu software

write_csv(loc_rides_start , file= "~/Bike_data_location_based.csv" )

## Visualize and gain more insight


# Start to visualize the data 

#loading the ggplot for creating charts 
library(ggplot2)

# create a col plot to show the avg time per user type using the service 
bike_rides_2021 %>% group_by(member_casual) %>% summarise(avg = mean(ride_length)) %>%
  ggplot( aes(x =member_casual , y = avg  , fill = member_casual))+
  geom_col() +
  labs(title = "Avarage Time on Bike per User Type"  , x = "Avarage Time of Ride" , y = "User Type" , fill = "User Type")

# view the same type of data but by week day to understand the behavior through out the week 
bike_rides_2021 %>% group_by(day_of_week , member_casual) %>% summarise(number_rides = n_distinct(ride_id)) %>% 
  ggplot( aes ( x = day_of_week , y = number_rides , fill = member_casual ))+
  geom_col(position = "dodge")+
  labs(title = "Number of Bike Rides by User Type During the Week"  , x = "Day of the Week" , y = "Number of Rides" , fill = "User Type")

# creating a density plot to understand high density ride length value  
density <- bike_rides_2021 %>% filter(ride_length<3600)  %>%
  ggplot( aes(x = ride_length , fill = member_casual)) +
  geom_density()+
  labs(title = "Density of  Ride Times for the User Groups"  , x = "Ride Time (length)" , y = "Density" , fill = "User Type")

histogram <- bike_rides_2021 %>% filter(ride_length<3600)  %>%
  ggplot( aes(x = ride_length , fill = member_casual)) +
  geom_histogram()+
  labs(title = "HistoGram of  Ride Times for the User Groups"  , x = "Ride Time (length)" , y = "Density" , fill = "User Type")

grid.arrange(density , histogram , ncol =2 )

### Analysis and Observations : 

# - Casual users have a higher avg of ride length service members 
# - The density plot shows that most riders for both members and casual riders usually use the service for less than 10 mins on avarage 
# - The members seem to be utilizing the service consistently through out the week while the casual users utilize it more on the weekends 


# ACT and Recommendation Step

# based on the observations from the data we can conclude the following:
#   
#   The marketing campaign should focus on the list of members with the 5 - 10 mins ride times and further deep dive into their habits
# 
# perhaps some promotions for extending the their rides to have more time using the service
# 
# Weekend centric promotions or service discounts for during week non members
# 
# further deeper analysis should be done to understand the location and route behavior the user take , that can lead to more thoughtful insights on the station placement.
# 
# further plots and more extended analysis will be provided at Cyclistic bike-share analysis in Tableau ( https://public.tableau.com/app/profile/jihad.al.hussain/viz/Cyclisticbike-shareanalysis_16413688581510/Dashboard1)
