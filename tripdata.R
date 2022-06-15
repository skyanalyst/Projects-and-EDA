## CYCLIST CASE STUDY_1
============================

## IMPORTING LIBRARIES
======================
library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()

## IMPORTING CSV FILES
=======================

df<- read_csv("tripdata.csv")
View(tripdata)

## RENAME COLUMNS
==================

str(tripdata)
library(dplyr)
tripdata<-rename(tripdata,bike_type=rideable_type
                 ,subscriber_type=member_casual)
View(tripdata)

## CLEAN DATA FOR ANALYSIS
============================

dim(tripdata)
colnames(tripdata)
summary(tripdata)
head(tripdata)
table(tripdata$subscriber_type)


tripdata$date<-as.Date(tripdata$started_at)
tripdata$month<-format(as.Date(tripdata$date),"%m")
tripdata$day<-format(as.Date(tripdata$date),"%d")
tripdata$year<-format(as.Date(tripdata$date),"%Y")
tripdata$day_of_week<-format(as.Date(tripdata$date),"%A")
View(tripdata)

## ADD NEW COLUMN RIDE_LENGTH TO CALCULATE ALL TRIPS
=====================================================
tripdata$ride_length<-difftime(tripdata$ended_at,tripdata$started_at,units = "mins")
str(tripdata)

##DESCRIPTIVE ANALYSIS
======================

# Descriptive analysis on ride_length column

mean(tripdata$ride_length)
median(tripdata$ride_length)
max(tripdata$ride_length)
min(tripdata$ride_length)

summary(tripdata$ride_length)

# Compare member and casual user

aggregate(tripdata$ride_length~tripdata$subscriber_type,FUN = mean)
aggregate(tripdata$ride_length~tripdata$subscriber_type,FUN = median)
aggregate(tripdata$ride_length~tripdata$subscriber_type,FUN = max)
aggregate(tripdata$ride_length~tripdata$subscriber_type,FUN = min)

# Look the average ride time by each day for members vs casual users

aggregate(tripdata$ride_length~tripdata$subscriber_type+tripdata$day_of_week,FUN = mean)

# Analyze ridership data by type and weekday

library(lubridate)
tripdata%>%mutate(weekday=wday(started_at,label=TRUE))%>%group_by(subscriber_type,weekday)%>%summarise(number_of_rides=n(),average_duration=mean(ride_length))%>%arrange(subscriber_type,weekday)



## VISUALIZATION
=================

# Number of rides by biketype  

tripdata%>%mutate(weekday=wday(started_at,label=TRUE))%>%group_by(subscriber_type,weekday)%>%summarise(number_of_rides=n(),average_duration=mean(ride_length))%>%arrange(subscriber_type,weekday)%>%ggplot(aes(x=weekday,y=number_of_rides,fill=subscriber_type))+geom_col(position = "dodge")

# Average duration

tripdata%>%mutate(weekday=wday(started_at,label=TRUE))%>%group_by(subscriber_type,weekday)%>%summarise(number_of_rides=n(),average_duration=mean(ride_length))%>%arrange(subscriber_type,weekday)%>%ggplot(aes(x=weekday,y=average_duration,fill=subscriber_type))+geom_col(position = "dodge")

## SAVE AS CSV FILE
=====================
  
write.csv(tripdata,'tripdata.csv')

