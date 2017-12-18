##1. Code for reading in the dataset and/or processing the data
#Add Libraries and set up working Directory, download the source/data              file,unzip the file, read the data file, look at what data looks like

knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(dplyr)
library(ggplot2)
path<- file.path(getwd(),"activity_monitoring.zip")
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, path, method="libcurl")
unzip(path)
dir()
data<- read.csv("activity.csv",header=TRUE,sep=',')
head(data)
str(data)
summary(data)

#Tidying the data

library(lubridate)
data$steps <- as.numeric(data$steps)
data$date<- as.character(data$date)
data$date <- ymd(data$date)
data$interval<-as.numeric(data$interval)
sum_steps_daily <- data%>%group_by(date)%>% summarise(sumsteps=sum(steps,na.rm=TRUE))
head(sum_steps_daily)
## 2.Histogram of the total number of steps taken each day
#Use hist function to Plot histogram of Steps 

with(sum_steps_daily,hist(sumsteps,col= "Blue", xlab= "daily steps", main="Histogram of daily steps"))

##3. To find mean and median of the steps taken per day

steps_mean <- mean(sum_steps_daily$sumsteps,na.rm=TRUE)
steps_median <-median(sum_steps_daily$sumsteps,na.rm=TRUE)
print( steps_mean)
print(steps_median)

##4. Daily average activity pattern

library(ggplot2)
fivemts_averages <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval), 
                              FUN = mean, na.rm = TRUE)

ggplot(data = fivemts_averages, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + 
  ylab("average number of steps taken")

##5. To find Max activity of steps in a 5minute interval

fivemts_averages[which.max(fivemts_averages$steps),]

##6.Imputing missing values with daily avearge in 5 minute inetrval
#As indicated through summary of data there are 2304 NAs in variable steps

nodata<-is.na(data$steps)
int_average<-tapply(data$step,data$interval,FUN=mean,na.rm=TRUE)
data_wnas<- data
data_wnas$steps[nodata]<-int_average[as.character(data_wnas$interval[nodata])]
summary(data_wnas)

##7. To compute average steps and histogram after removing NAs


steps_wnas <- data_wnas %>%
  group_by(date) %>%
  summarize(sum_steps = sum(steps)) %>%
  print
with(steps_wnas,hist(sum_steps,col= "violet", xlab= "daily steps",
                     main="Histogram of daily steps after replacing NAs"))

#compute mean and median values


mean_steps_wnas <-mean(steps_wnas$sum_steps,na.rm=TRUE)
mean_steps_wnas
median_steps_wnas <- median(steps_wnas$sum_steps,na.rm=TRUE)
median_steps_wnas

##8. Panel plot for steps on weekdays,weekends

#using function weekdays and data with replaced NA's

data_wnas$date <- as.Date(data_wnas$date)
data_daytype<- with(data_wnas,mutate(data_wnas,Daytype=ifelse(weekdays(data_wnas$date) =="Saturday"| weekdays(data_wnas$date)=="Sunday" , "Weekend", "Weekday")))
data_daytype$Daytype <- as.factor(data_daytype$Daytype)
steps_daytype <- data_daytype %>%
group_by(Daytype,interval) %>%
summarize(avg_steps = mean(steps)) 
str(steps_daytype)
g<-ggplot(steps_daytype, aes(x=interval, y=avg_steps, color = Daytype)) +
  geom_line() +
  facet_wrap(~Daytype, ncol = 2, nrow=1)
print(g)