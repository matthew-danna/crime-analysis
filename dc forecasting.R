library(tidyverse)
library(zoo)
library(aTSA)
library(tseries)
library(forecast)
library(leaflet)
library(sf)
library(tigris)

# https://opendata.dc.gov/datasets/89561a4f02ba46cca3c42333425d1b87/explore?location=38.904018%2C-77.012050%2C12.85
# https://www.arcgis.com/sharing/rest/content/items/89561a4f02ba46cca3c42333425d1b87/info/metadata/metadata.xml?format=default&output=html
# https://crimecards.dc.gov/

dc.pastyear <- read.csv("https://datagate.dc.gov/search/open/crimes?daterange=1year%20to%20date&details=true&format=csv", stringsAsFactors = FALSE)
dc.8years <- read.csv("https://datagate.dc.gov/search/open/crimes?daterange=8years&details=true&format=csv", stringsAsFactors = FALSE)

library(data.table)
dc.test2023 <- fread("https://datagate.dc.gov/search/open/crimes?daterange=1-1-2023,9-12-2023&details=true&format=csv")
dc.test2023b <- fread("https://datagate.dc.gov/search/open/crimes?daterange=1-1-2023%20to%20date&details=true&format=csv")
dc.test2022 <- fread("https://datagate.dc.gov/search/open/crimes?daterange=1-1-2022,12-31-2022&details=true&format=csv")
dc.test2021 <- fread("https://datagate.dc.gov/search/open/crimes?daterange=1-1-2021,12-31-2021&details=true&format=csv")

dc.data2023 <- read.csv("https://opendata.arcgis.com/datasets/89561a4f02ba46cca3c42333425d1b87_5.csv", stringsAsFactors = FALSE) 
dc.data.past30 <- read.csv("https://opendata.arcgis.com/datasets/dc3289eab3d2400ea49c154863312434_8.csv", stringsAsFactors = FALSE)
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

dc.data.temp <- rbind(dc.data2008, dc.data2009, dc.data2010, dc.data2011, dc.data2012, dc.data2013, dc.data2014, dc.data2015, 
                      dc.data2016, dc.data2017, dc.data2018, dc.data2019, dc.data2020, dc.data2021, dc.data2022, dc.data2023)
dc.data <- separate(dc.data.temp, REPORT_DAT, into = c("date", "time"), sep = " ")
dc.data$date <- as.Date(dc.data$date, format = "%Y/%m/%d")

dc.data$year <- substr(dc.data$date, 0, 4)
dc.data$month <- month(dc.data$date)
dc.data$day <- day(dc.data$date)
dc.data$dow <- weekdays(dc.data$date)
dc.data$hour <- substr(dc.data$time, 0, 2)

unique(dc.data$OFFENSE)
offense.robbery <- subset(dc.data, dc.data$OFFENSE == 'ROBBERY')

unique(dc.data$METHOD)
data <- subset(dc.data, dc.data$METHOD == 'GUN')

unique(dc.data$SHIFT)
data <- subset(dc.data, dc.data$SHIFT == 'MIDNIGHT')

unique(dc.data$DISTRICT)
data <- subset(dc.data, dc.data$DISTRICT == '7')

# Step 6:
crime.day <- dc.data %>%
  group_by(date) %>%
  summarise(count = n())

### FILL IN THE BLANK DAYS
calls.day <- calls.day %>% 
  complete(date = seq(ymd(first), 
                      ymd(last), 
                      "day")) %>%
  mutate(weekday = wday(date, label = T, week_start = 1), 
         month = month(date, label = T, abbr = F),
         week = isoweek(date),
         day = day(date),
         year = year(date))

# Step 7:
graph.day <- ggplot(crime.day, aes(x = date, y = count)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  xlab("Months") +
  ylab("All Crime") +
  ggtitle("All the D.C. Crimes, since 2008") +
  geom_area(fill = "lightblue", color = "black")

graph.day +
  geom_smooth(method = lm, col = "red", se = FALSE)

# Step 8: 
data.cleaned <- zoo(crime.day$count, seq(from = as.Date(min(crime.day$date)), to = as.Date(max(crime.day$date)-1), by = 1))

# Step 9: 
summary(data.cleaned)
plot(data.cleaned)


stationary.data <- diff(data.cleaned)

# Step 11:
adf.test(as.matrix(stationary.data))

# Step 12:
arima.function <- auto.arima(stationary.data)

# Step 13:
forecast.7days <- forecast(arima.function, h=7)
additional.7days <- round(sum(forecast.7days$upper[,2]),0)
additional.7days

# Step 14: 
total.7days <- round(sum(crime.day$count) + additional.7days, 0)
plot(forecast.7days)
title("Test")

# Step 15:
# 1-day forecast:
forecast.1days <- forecast(arima.function, h=1)
additional.1days <- round(sum(forecast.1days$upper[,2]),0)
total.1days <- round(sum(crime.day$count) + additional.1days, 0)
plot(forecast.1days)

##### Add maps??

###### Add temporal topology

