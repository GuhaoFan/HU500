
install.packages("sqldf")
install.packages("geosphere")
install.packages("lubridate")

###Randomly Selected Data Base with No of Obs = 1000
library(sqldf)
data0 = read.csv.sql("~/Downloads/new-york-city-taxi-fare-prediction/train.csv", sql = "select * from file order by random() limit 1000")


###Data to begin
dat0 = data0

###Prepare the data with additional calculated variables 
##Distance
library(geosphere)
dat0$dist = distHaversine(dat0[,c('pickup_longitude','pickup_latitude')], dat0[,c('dropoff_longitude','dropoff_latitude')])

##Break down the pickup Day and Time
dat0$pickup_day = weekdays(as.Date(dat0$pickup_datetime))
dat0$pickup_time = as.numeric(as.POSIXct(dat0$pickup_datetime))
library(stringr)
library(lubridate)
library(plyr)
dat0$pickup_hour = hour(dat0$pickup_datetime)
library(Hmisc)
dat0$hour_group = cut2(dat0$pickup_hour,c(6,11,16,20))
levels(dat0$hour_group)
dat0$day_group = ifelse(dat0$pickup_day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")


###First Base Data to fit regression
dat1 = dat0[,-c(1,3:7,12:14)]
str(dat1)
dat1$pickup_day = factor(dat1$pickup_day)
dat1.numeric = dat1[c(-4)]
pairs(dat1)
cor(dat1.numeric)

fit1 = lm(fare_amount~., data=dat1)
summary(fit1)
anova(fit1)

###Second Base Data to fit regression
dat2 = dat0[,-c(1,3:7,10:12)]
str(dat2)
dat2$hour_group = factor(dat2$hour_group)
dat2$day_group = factor(dat2$day_group)
dat2.numeric = dat2[-c(4:5)]
pairs(dat2)
cor(dat2.numeric)

fit2 = lm(fare_amount~., data=dat2)
summary(fit2)
anova(fit2)
