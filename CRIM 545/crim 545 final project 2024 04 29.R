# packages
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)
library(zoo)
library(aTSA)
library(tseries)
library(forecast)

# data
arrests <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                          "1YdW0ov1OIm9MNmPLIomfpAy8RO4p6gxG"))
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                          "1X48tHqgfDVTbPTQ4wKxVPFazLyGjOfxz"))
crimes <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                          "1Sg7lkf6EioI87dZiGDsn03un8gxT4zw6"))
crashes <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                          "118xvIlrqOQFsrPVh35CvPmPkNXElenK1"))

# format date as a date
arrests$date <- as.Date(arrests$date)
calls$date <- as.Date(calls$date)
crashes$date <- as.Date(crashes$date)
crimes$date <- as.Date(crimes$date.report)

# this is probably a bad idea
summary.arrests <- arrests %>%
  group_by(crime.code.description) %>%
  summarise(count = n())

# these only really work with you use all of them
summary.crashes <- crashes %>%
  group_by(type) %>%
  summarise(count = n())

# if you use this, use combinations of crime types
summary.crime <- crimes %>%
  group_by(type) %>%
  summarise(count = n())

summary.calls <- calls %>%
  group_by(type) %>%
  summarise(count = n())

# subsets
sub.arrests <- subset(arrests, 
                      arrests$crime.code.description == 'AGGRAVATED ASSAULT' |
                        arrests$crime.code.description == 'ALL OTHER OFFENSES')

# build forecast model from Project 3 here
### adjust this as necessary!
crime <- arrests %>%
  group_by(date) %>%
  summarise(COUNT = n())

crime <- crime %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  mutate(WEEKDAY = lubridate::wday(date, label = T, week_start = 1), 
         MONTH = lubridate::month(date, label = T, abbr = F),
         WEEK = isoweek(date),
         DAY = day(date),
         YEAR = year(date))

crime <- replace(crime, is.na(crime), 0)

cleaned.data <- zoo(crime$COUNT, 
                    seq(from = as.Date(min(crime$date)), 
                        to = as.Date(max(crime$date)-1), by = 1))

summary(cleaned.data)
plot(cleaned.data)
title("Insert A Title Here") # this adds a title to your graph

stationary.data <- diff(cleaned.data)
plot(stationary.data)

adf.test(as.matrix(stationary.data)) 

arima.data <- auto.arima(stationary.data)
arima.data

month01 <- as.Date("2024-06-01")
month30 <- as.Date("2024-06-30")
start <- as.numeric(month01 - max(crime$date))
end <- as.numeric(month30 - max(crime$date))
days <- c(start:end)

forecast00 <- forecast(arima.data, h=(days[1]-1))
forecast00 <- round(sum(forecast00$upper[,2]),0)
forecast01 <- forecast(arima.data, h=days[1])
forecast01 <- round(sum(forecast01$upper[,2]),0)
forecast02 <- forecast(arima.data, h=days[2])
forecast02 <- round(sum(forecast02$upper[,2]),0)
forecast03 <- forecast(arima.data, h=days[3])
forecast03 <- round(sum(forecast03$upper[,2]),0)
forecast04 <- forecast(arima.data, h=days[4])
forecast04 <- round(sum(forecast04$upper[,2]),0)
forecast05 <- forecast(arima.data, h=days[5])
forecast05 <- round(sum(forecast05$upper[,2]),0)
forecast06 <- forecast(arima.data, h=days[6])
forecast06 <- round(sum(forecast06$upper[,2]),0)
forecast07 <- forecast(arima.data, h=days[7])
forecast07 <- round(sum(forecast07$upper[,2]),0)
forecast08 <- forecast(arima.data, h=days[8])
forecast08 <- round(sum(forecast08$upper[,2]),0)
forecast09 <- forecast(arima.data, h=days[9])
forecast09 <- round(sum(forecast09$upper[,2]),0)
forecast10 <- forecast(arima.data, h=days[10])
forecast10 <- round(sum(forecast10$upper[,2]),0)
forecast11 <- forecast(arima.data, h=days[11])
forecast11 <- round(sum(forecast11$upper[,2]),0)
forecast12 <- forecast(arima.data, h=days[12])
forecast12 <- round(sum(forecast12$upper[,2]),0)
forecast13 <- forecast(arima.data, h=days[13])
forecast13 <- round(sum(forecast13$upper[,2]),0)
forecast14 <- forecast(arima.data, h=days[14])
forecast14 <- round(sum(forecast14$upper[,2]),0)
forecast15 <- forecast(arima.data, h=days[15])
forecast15 <- round(sum(forecast15$upper[,2]),0)
forecast16 <- forecast(arima.data, h=days[16])
forecast16 <- round(sum(forecast16$upper[,2]),0)
forecast17 <- forecast(arima.data, h=days[17])
forecast17 <- round(sum(forecast17$upper[,2]),0)
forecast18 <- forecast(arima.data, h=days[18])
forecast18 <- round(sum(forecast18$upper[,2]),0)
forecast19 <- forecast(arima.data, h=days[19])
forecast19 <- round(sum(forecast19$upper[,2]),0)
forecast20 <- forecast(arima.data, h=days[20])
forecast20 <- round(sum(forecast20$upper[,2]),0)
forecast21 <- forecast(arima.data, h=days[21])
forecast21 <- round(sum(forecast21$upper[,2]),0)
forecast22 <- forecast(arima.data, h=days[22])
forecast22 <- round(sum(forecast22$upper[,2]),0)
forecast23 <- forecast(arima.data, h=days[23])
forecast23 <- round(sum(forecast23$upper[,2]),0)
forecast24 <- forecast(arima.data, h=days[24])
forecast24 <- round(sum(forecast24$upper[,2]),0)
forecast25 <- forecast(arima.data, h=days[25])
forecast25 <- round(sum(forecast25$upper[,2]),0)
forecast26 <- forecast(arima.data, h=days[26])
forecast26 <- round(sum(forecast26$upper[,2]),0)
forecast27 <- forecast(arima.data, h=days[27])
forecast27 <- round(sum(forecast27$upper[,2]),0)
forecast28 <- forecast(arima.data, h=days[28])
forecast28 <- round(sum(forecast28$upper[,2]),0)
forecast29 <- forecast(arima.data, h=days[29])
forecast29 <- round(sum(forecast29$upper[,2]),0)
forecast30 <- forecast(arima.data, h=days[30])
forecast30 <- round(sum(forecast30$upper[,2]),0)

forecast.list <- c(forecast00, forecast01, forecast02, forecast03, 
                   forecast04, forecast05, forecast06, forecast07, 
                   forecast08, forecast09, forecast10, forecast11, 
                   forecast12, forecast13, forecast14, forecast15, 
                   forecast16, forecast17, forecast18, forecast19, 
                   forecast20, forecast21, forecast22, forecast23, 
                   forecast24, forecast25, forecast26, forecast27, 
                   forecast28, forecast29, forecast30)

forecasts.tmp <- data.frame(c(start-1,days))
colnames(forecasts.tmp) <- "DAYS"
forecasts.tmp$ID <- seq.int(nrow(forecasts.tmp))
forecasts.tmp$DATE <- forecasts.tmp$DAYS + max(crime$date)
forecasts.tmp$TOTAL <- forecast.list
forecasts <- subset(forecasts.tmp, forecasts.tmp$ID > 1)
forecasts$ID <- seq.int(nrow(forecasts))
forecasts.tmp <- forecasts.tmp[c(2,4)]
names(forecasts.tmp) <- c("ID", "YESTERDAY")
forecasts <- forecasts %>% left_join(forecasts.tmp, by = "ID")

forecasts$TODAY <- forecasts$TOTAL - forecasts$YESTERDAY
forecasts <- forecasts[c(2,3,6)]

# subset my forecast data for all prior June's

# make hotspot maps
### get Fairfax streets and city outline datas

# make temporal topology

