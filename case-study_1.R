#cyclist full year analysis
#==========================

#load packages
#=============

library(tidyverse)#for calculation
library(ggplot2)#for visualisation
library(lubridate)#dates

#claening unnecesary files
#===========================
df <- tripdata%>%select(-c(start_lat,start_lng,end_lat,end_lng))
view(final_tripdata)


## CLEAN UP AND ADD DATA FOR ANALYSIS
#=====================================

#LIST OF COLUMN NAMES
#====================
colnames(df)

#HOW MANY ROWS ARE IN DATA FRAME ?
#=================================
nrow(df)

#DIMENSIO NS OF DATAFRAME?
dim(df)

#SEE THE FIRST 6 ROWS OF DATAFRAME
head(df)

#LAST 6 ROWS OF DATAFRAME
tail(df)

#SEE LIST OF COLUMNS AND DATA TYPES
str(df)

# STATISTICAL SUMMARY OF DATA
summary(df)

#IN THE "MEMBER_CASUAL" COLUMN , REPLACE "SUBSCRIBER' WITH "MEMBER"  AND "CUSTOMER" WITH "CASUAL"
df<- df%>%mutate(member_casual=recode(member_casual,"subscriber"="member","customer"="casual"))

# check to make sure the proper number of observation was assigned
table(df$member_casual)

#Add columns that list the date, month,day and year fo each ride
#THIS WILL ALLOW US TO AGGREGATE RIDE DATA FOR EACH MONTH, DAY OR YEAR...BEFORE COMPLETING THESE OPERATION WE COULD ONLY AGGREGATE AT THE RIDE
# http://www.statmethods.net/input/dates.html more on date formats in R found at that link
df$date<-as.Date(final_tripdata$started_at)#THE DEFAULT FORMAT IS YYYY-MM-DD
df$month<- format(as.Date(final_tripdata$date),"%m")
df$day<- format(as.Date(final_tripdata$date),"%d")
df$year<- format(as.Date(final_tripdata$date),"%Y")
df$day_of_week<- format(as.Date(final_tripdata$date),"%A")

#Add a 'ride_length'calculatons to final_tripdata (in seconds)
#===============================================================
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
df$ride_length<-difftime(df$ended_at,df$started_at,units = "mins")

#Inspect the structure of the columns
str(df)

#Convert 'ride_length' from factor to numeric so we can run calculations on the data
#========================================================================================
is.factor(df$ride_length)
df$ride_length<-as.numeric(as.character(df$ride_length))
is.numeric(df$ride_length)

#Remove na and duplicate files
#=============================
df<-na.omit(df)#remove NA files
df<-distinct(df)#remove duplicate files
df<-df[!(df$ride_length<=0)]#remove ride_length whose value is 0 or less than zero


##CONDUCT DESCRIPTIVE ANALYSIS
#===============================

#Descriptive analysis on ride_length(all figures in second)
mean(df$ride_length) #straight average (total ride length/rides)
median(df$ride_length)#midpoint nuber in the ascending array of ride lengths
max(df$ride_length)#longest ride
min(df$ride_length)#shortest ride

#You can condense the four lines above to one line using summary() on the specific attribute
aggregate(df$ride_length~df$member_casual,FUN = mean)
summary(df$ride_length)

#compare members and casual users
aggregate(df$ride_length~df$member_casual,FUN = mean)
aggregate(df$ride_length~df$member_casual,FUN = median)
aggregate(df$ride_length~df$member_casual,FUN = max)
aggregate(df$ride_length~df$member_casual,FUN = min)

#See the average ride time by each day for members vs casual users
aggregate(df$ride_length~df$member_casual+df$day_of_week,FUN = mean)

#Notice that the days of the week are out of order.Let's fix that
df$day_of_week<- ordered(df$day_of_week,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#Now, let's run the average ride time by each day for members vs casual users
aggregate(df$ride_length~df$member_casual+df$day_of_week, FUN = mean)

#analyze ridership data by type and weekday
#==========================================
df%>%
  mutate(weekday=wday(started_at,label = TRUE))%>% #creates weekday field using wday()
  group_by(member_casual,weekday)%>% #groups y user type and weekday
  summarise(number_of_rides=n()
    ,average_duration=mean(ride_length))%>% #calculates the average duration
     arrange(member_casual,weekday) #sorts  

#VISUALISATION
#==============

#Let's create a visualisation for average duration
#--------------------------------------------------
df%>%
  mutate(weekday=wday(started_at,label = TRUE))%>% 
  group_by(member_casual,weekday)%>%
  summarise(number_of_rides=n()
            ,average_duration=mean(ride_length))%>%
  arrange(member_casual,weekday)%>%
  ggplot(mapping = aes(x=weekday,y=number_of_rides,fill=member_casual))+
  geom_col(position = "dodge")


#Let's create a visualisation for average_duration
#---------------------------------------------------
df%>%
  mutate(weekday=wday(started_at,label = TRUE))%>% 
  group_by(member_casual,weekday)%>%
  summarise(number_of_rides=n()
            ,average_duration=mean(ride_length))%>%
  arrange(member_casual,weekday)%>%
  ggplot(mapping = aes(x=weekday,y=average_duration,fill=member_casual))+
  geom_col(position = "dodge")



# EXPORT SUMMARY FOR FURTHER ANALYSIS
#====================================
cyclistic<-aggregate(df$ride_length~df$member_casual+df$day_of_week,FUN = mean)

write.csv(cyclistic,"cyclistic.csv")



