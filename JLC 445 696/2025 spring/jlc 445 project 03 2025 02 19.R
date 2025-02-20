##### Step 0
install.packages('tidyverse')
install.packages('data.table')
install.packages('leaflet')
install.packages('sf')
install.packages('tigris')
install.packages('zoo')
install.packages('tseries')
install.packages('aTSA')
install.packages('forecast')

library(tidyverse)
library(leaflet)
library(sf)
library(tigris)
library(zoo)
library(aTSA)
library(tseries)
library(forecast)
library(data.table)

##### Step 1 (steps 1-3 from Project 2)
url <- "https://opendata.dc.gov/datasets/DCGIS::crime-incidents-in-"
years <- c(2008:2025)
full.urls <- paste0(url, years, ".csv")
dc.data <- data.frame()

for(file in full.urls) {
  tmp.data <- read.csv(file, stringsAsFactors = FALSE)
  dc.data <- rbind(tmp.data, dc.data)
}

dc.data <- separate(dc.data, REPORT_DAT, into = c("DATE", "TIME"), sep = " ")
dc.data$DATE <- as.Date(dc.data$DATE, format = "%Y/%m/%d")
dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$MONTHS <- months(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)
dc.data$HOUR <- substr(dc.data$TIME, 0, 2)

##### Step 2
unique(dc.data$OFFENSE)
unique(dc.data$SHIFT)

summary.offense <- dc.data %>%
  group_by(OFFENSE) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

summary.shift <- dc.data %>%
  group_by(SHIFT, OFFENSE) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

# make subsets
dc1 <- subset(dc.data, dc.data$OFFENSE == 'ROBBERY' &
                dc.data$SHIFT == 'DAY')
dc2 <- subset(dc.data, dc.data$OFFENSE == 'xxx' &
                dc.data$SHIFT == 'EVENING')
dc3 <- subset(dc.data, dc.data$OFFENSE == 'yyy' &
                dc.data$SHIFT == 'MIDNIGHT')

# model
crime <- dc1 %>%
  group_by(DATE) %>%
  summarise(COUNT = n())

crime <- crime %>%
  complete(DATE = seq.Date(min(DATE), max(DATE), by = "day")) %>%
  mutate(WEEKDAY = lubridate::wday(DATE, label = T, week_start = 1), 
         MONTH = lubridate::month(DATE, label = T, abbr = F),
         WEEK = isoweek(DATE),
         DAY = day(DATE),
         YEAR = year(DATE))
crime <- replace(crime, is.na(crime), 0)

cleaned.data <- zoo(crime$COUNT, 
                    seq(from = as.Date(min(crime$DATE)), 
                        to = as.Date(max(crime$DATE)-1), by = 1))

summary(cleaned.data)
plot(cleaned.data)

stationary1 <- diff(cleaned.data, differences = 1)
stationary2 <- diff(cleaned.data, differences = 2)
stationary3 <- diff(cleaned.data, differences = 3)

plot(cleaned.data)
plot(stationary1)
plot(stationary2)
plot(stationary3)

adf.test(as.matrix(cleaned.data))
adf.test(as.matrix(stationary1))
adf.test(as.matrix(stationary2))
adf.test(as.matrix(stationary3)) 

pacf(stationary1)
pacf(stationary1, pl=FALSE)

acf(stationary1)
acf(stationary1, pl=FALSE)

arima.data <- auto.arima(cleaned.data, 
                         d = 1, max.p = 10, max.q = 1, 
                         seasonal = T)
summary(arima.data)

checkresiduals(arima.data)

# h = the number of units of time to measure
forecast.7days <- forecast(arima.data, h=7)
round(sum(forecast.7days$upper[,2]),0)
forecast.7days$mean
