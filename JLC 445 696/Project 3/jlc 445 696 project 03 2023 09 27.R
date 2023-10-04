# Step 0: packages

install.packages('tidyverse')
install.packages('leaflet')
install.packages('sf')
install.packages('tigris')
install.packages('zoo')
install.packages('tseries')
install.packages('aTSA')
install.packages('forecast')
install.packages('data.table')
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)
library(zoo)
library(aTSA)
library(tseries)
library(forecast)
library(data.table)

# Step 1: Data
dc.data2023.temp <- fread("https://datagate.dc.gov/search/open/crimes?daterange=1-1-2023%20to%20date&details=true&format=csv")
dc.data2022 <- read.csv("https://opendata.arcgis.com/datasets/f9cc541fc8c04106a05a1a4f1e7e813c_4.csv", stringsAsFactors = FALSE)
dc.data2021 <- read.csv("https://opendata.arcgis.com/datasets/619c5bd17ca2411db0689bb0a211783c_3.csv", stringsAsFactors = FALSE)
dc.data2020 <- read.csv("https://opendata.arcgis.com/datasets/f516e0dd7b614b088ad781b0c4002331_2.csv", stringsAsFactors = FALSE)
dc.data2019 <- read.csv("https://opendata.arcgis.com/datasets/f08294e5286141c293e9202fcd3e8b57_1.csv", stringsAsFactors = FALSE)
dc.data2018 <- read.csv("https://opendata.arcgis.com/datasets/38ba41dd74354563bce28a359b59324e_0.csv", stringsAsFactors = FALSE)
dc.data2017 <- read.csv("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.csv", stringsAsFactors = FALSE)
dc.data2016 <- read.csv("https://opendata.arcgis.com/datasets/bda20763840448b58f8383bae800a843_26.csv", stringsAsFactors = FALSE)
dc.data2015 <- read.csv("https://opendata.arcgis.com/datasets/35034fcb3b36499c84c94c069ab1a966_27.csv", stringsAsFactors = FALSE)
dc.data2014 <- read.csv("https://opendata.arcgis.com/datasets/6eaf3e9713de44d3aa103622d51053b5_9.csv", stringsAsFactors = FALSE)
dc.data2013 <- read.csv("https://opendata.arcgis.com/datasets/5fa2e43557f7484d89aac9e1e76158c9_10.csv", stringsAsFactors = FALSE)
dc.data2012 <- read.csv("https://opendata.arcgis.com/datasets/010ac88c55b1409bb67c9270c8fc18b5_11.csv", stringsAsFactors = FALSE)
dc.data2011 <- read.csv("https://opendata.arcgis.com/datasets/9d5485ffae914c5f97047a7dd86e115b_35.csv", stringsAsFactors = FALSE)
dc.data2010 <- read.csv("https://opendata.arcgis.com/datasets/fdacfbdda7654e06a161352247d3a2f0_34.csv", stringsAsFactors = FALSE)
dc.data2009 <- read.csv("https://opendata.arcgis.com/datasets/73cd2f2858714cd1a7e2859f8e6e4de4_33.csv", stringsAsFactors = FALSE)
dc.data2008 <- read.csv("https://opendata.arcgis.com/datasets/180d56a1551c4e76ac2175e63dc0dce9_32.csv", stringsAsFactors = FALSE)

# clean 2008-2022
dc.data.temp <- rbind(dc.data2008, dc.data2009, dc.data2010, dc.data2011, dc.data2012, dc.data2013, dc.data2014, dc.data2015, dc.data2016, dc.data2017, dc.data2018, dc.data2019, dc.data2020, dc.data2021, dc.data2022)
dc.data.temp <- separate(dc.data.temp, REPORT_DAT, into = c("DATE", "TIME"), sep = " ")
dc.data.temp <- dc.data.temp %>%
  select(3:9,12:22)
dc.data.temp$DATE <- as.Date(dc.data.temp$DATE, format = "%Y/%m/%d")
dc.data.temp$NEIGHBORHOOD_CLUSTER <- toupper(dc.data.temp$NEIGHBORHOOD_CLUSTER)
dc.data.temp$HOUR <- substr(dc.data.temp$TIME, 0, 2)

# clean 2023
dc.data2023.temp2 <- separate(dc.data2023.temp, REPORT_DAT, into = c("DATE", "TIME"), sep = ", ")
# clean the 2023 AM times
dc.data2023.temp3 <- separate(dc.data2023.temp2, TIME, into = c("TIME", "AM.PM"), sep = " ")
dc.data2023.am <- subset(dc.data2023.temp3, dc.data2023.temp3$AM.PM == 'AM')
dc.data2023.am$HOUR <- substr(dc.data2023.am$TIME, 0, 2)
dc.data2023.am$HOUR <- gsub(":", "", dc.data2023.am$HOUR)
dc.data2023.am$HOUR <- paste("0",dc.data2023.am$HOUR, sep = "")
dc.data2023.am$HOUR <- str_sub(dc.data2023.am$HOUR, start = -2)
dc.data2023.am$HOUR <- gsub("12", "00", dc.data2023.am$HOUR)
# clean the 2023 PM times
dc.data2023.pm <- subset(dc.data2023.temp3, dc.data2023.temp3$AM.PM == 'PM')
dc.data2023.pm$HOUR <- substr(dc.data2023.pm$TIME, 0, 2)
dc.data2023.pm$HOUR <- gsub(":", "", dc.data2023.pm$HOUR)
dc.data2023.pm$HOUR <- as.numeric(dc.data2023.pm$HOUR)
dc.data2023.pm$HOUR <- dc.data2023.pm$HOUR + 12
dc.data2023.pm$HOUR <- as.character(dc.data2023.pm$HOUR)
dc.data2023.pm$HOUR <- gsub("24", "12", dc.data2023.pm$HOUR)

dc.data2023 <- rbind(dc.data2023.am, dc.data2023.pm)

dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)

dc.data2023 <- subset(dc.data2023, select = c("CCN", "DATE", "TIME", "SHIFT", "METHOD", "OFFENSE", "BLOCK", "WARD", "ANC", "DISTRICT", "PSA", "NEIGHBORHOOD_CLUSTER", "BLOCK_GROUP", "CENSUS_TRACT", "VOTING_PRECINCT", "LATITUDE", "LONGITUDE", "BID", "HOUR"))
dc.data2023$SHIFT <- toupper(dc.data2023$SHIFT)
dc.data2023$METHOD <- toupper(dc.data2023$METHOD)
dc.data2023$OFFENSE <- toupper(dc.data2023$OFFENSE)
dc.data2023$BLOCK <- toupper(dc.data2023$BLOCK)
dc.data2023$NEIGHBORHOOD_CLUSTER <- toupper(dc.data2023$NEIGHBORHOOD_CLUSTER)
dc.data2023$DATE <- as.Date(dc.data2023$DATE, format = "%m/%d/%Y")

dc.data <- rbind(dc.data.temp, dc.data2023)

# Step 2
unique(dc.data$OFFENSE)
unique(dc.data$SHIFT)
unique(dc.data$DISTRICT)

summary.offense <- dc.data %>%
  group_by(OFFENSE) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

summary.shift <- dc.data %>%
  group_by(SHIFT) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

summary.district <- dc.data %>%
  group_by(DISTRICT) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

summary.offense.shift <- dc.data %>%
  group_by(OFFENSE,SHIFT) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

# data subsets
dc1 <- subset(dc.data, dc.data$OFFENSE == 'HOMICIDE' & dc.data$SHIFT == 'DAY')
dc2 <- subset(dc.data, dc.data$OFFENSE == 'SEX ABUSE' & dc.data$SHIFT == 'EVENING')
dc3 <- subset(dc.data, dc.data$OFFENSE == 'ARSON' & dc.data$SHIFT == 'MIDNIGHT')

# Step 3
crime <- dc.data %>%
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
title("Insert A Title Here") # this adds a title to your graph

stationary.data <- diff(cleaned.data)
plot(stationary.data)

adf.test(as.matrix(stationary.data)) 

arima.data <- auto.arima(stationary.data)
arima.data

forecast.7days <- forecast(arima.data, h = 7)
additional.7days <- round(sum(forecast.7days$upper[,2]),0)

# Daily forecasts
nov1 <- as.Date("2023-11-01")
nov30 <- as.Date("2023-11-30")
start <- as.numeric(nov1 - max(crime$DATE))
end <- as.numeric(nov30 - max(crime$DATE))
days <- c(start:end)

forecast00 <- forecast(arima.data, h=(days[1]-1))
forecast00 <- round(sum(forecast00$upper[,2]),0)
forecast01 <- forecast(arima.data, h=days[1])
forecast01 <- round(sum(forecast01$upper[,2]),0)
forecast02 <- forecast(arima.data, h=days[2])
forecast02 <- round(sum(forecast02$upper[,2]),0)





