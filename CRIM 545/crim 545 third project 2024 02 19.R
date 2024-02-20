# Step 0
install.packages('tidyverse')
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

# Step 1
dc.data2024 <- read.csv("https://opendata.arcgis.com/datasets/c5a9f33ffca546babbd91de1969e742d_6.csv", stringsAsFactors = FALSE)
dc.data2023 <- read.csv("https://opendata.arcgis.com/datasets/89561a4f02ba46cca3c42333425d1b87_5.csv", stringsAsFactors = FALSE)
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

dc.data.temp <- rbind(dc.data2010, dc.data2011, dc.data2012, dc.data2013,
                      dc.data2014, dc.data2015, dc.data2016, dc.data2017,
                      dc.data2018, dc.data2019, dc.data2020, dc.data2021,
                      dc.data2022, dc.data2023, dc.data2024)
dc.data <- separate(dc.data.temp, REPORT_DAT, into = c("date", "time"), sep = " ")
dc.data$date <- as.Date(dc.data$date, format = "%Y/%m/%d")
dc.data$year <- substr(dc.data$date, 0, 4)
dc.data$month <- month(dc.data$date)
dc.data$day <- day(dc.data$date)
dc.data$dow <- weekdays(dc.data$date)
dc.data$hour <- substr(dc.data$time, 0, 2)

# Step 2
unique(dc.data$OFFENSE)
unique(dc.data$SHIFT)

summary.offense <- dc.data %>%
  group_by(OFFENSE) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

summary.offense.shift <- dc.data %>%
  group_by(OFFENSE, SHIFT) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

dc.day <- subset(dc.data, dc.data$OFFENSE == 'MOTOR VEHICLE THEFT' & dc.data$SHIFT == 'DAY')
dc.evening <- subset(dc.data, dc.data$OFFENSE == 'xxx' & dc.data$SHIFT == 'EVENING')
dc.midnight <- subset(dc.data, dc.data$OFFENSE == 'yyy' & dc.data$SHIFT == 'MIDNIGHT')

# Step 3
crime <- dc.data %>%
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

stationary.data <- diff(cleaned.data)
plot(stationary.data)

adf.test(as.matrix(stationary.data))

arima.data <- auto.arima(stationary.data)
arima.data

forecast.7days <- forecast(arima.data, h=7)
additional.7days <- round(sum(forecast.7days$upper[,2]),0)

forecast.1days <- forecast(arima.data, h=1)
additional.1days <- round(sum(forecast.1days$upper[,2]),0)

forecast.365days <- forecast(arima.data, h=365)
additional.365days <- round(sum(forecast.365days$upper[,2]),0)

# Step 4
graph.crime <- ggplot(crime, aes(x=date, y=COUNT)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  xlab("Years") + 
  ylab("Crime") + 
  ggtitle("TITLE HERE") + 
  geom_area(fill="lightblue", color="black")
graph.crime

graph.crime + 
  geom_smooth(method = lm, col = "red", se = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

total.365days <- round(sum(crime$COUNT) + additional.365days, 0)
plot(forecast.365days)

## Maps
crime.april <- subset(dc.data, dc.data$month == '4')

## Temporal Topology
dc.april <- subset(dc.data, dc.data$month == '4')
crime.day.time <- dc.april %>%
  group_by(dow, hour) %>%
  summarise(COUNT = n())
crime.day.time <- subset(crime.day.time, !is.na(crime.day.time$hour))

ggplot(crime.day.time, aes(hour, dow, fill = COUNT)) +
  geom_tile()

ggplot(crime.day.time, aes(hour, dow, fill = COUNT)) +
  geom_tile() + coord_fixed()

ggplot(crime.day.time, aes(hour, dow, fill = COUNT)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + 
  coord_fixed()

ggplot(crime.day.time, aes(hour, dow, fill = COUNT)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = COUNT), color = "white", size = 1.5) +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_fixed()

ggplot(crime.day.time, aes(hour, dow, fill = COUNT)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = COUNT), color = "black", size = 2.5) +
  scale_fill_gradient(low = "blue", high = "red") +
  guides(fill = guide_colourbar(title = "Crime Count")) +
  coord_fixed()

ggplot(crime.day.time, aes(hour, dow, fill = COUNT)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = COUNT), color = "black", size = 1.5) +
  scale_fill_gradient(low = "white", high = "red") +
  guides(fill = guide_colourbar(title = "Crime Count", label = FALSE, ticks = FALSE)) +
  coord_fixed()

ggplot(crime.day.time, aes(hour, dow, fill = COUNT)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = COUNT), color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed()

ggplot(crime.day.time, aes(hour, dow, fill = COUNT)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = COUNT), color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_y_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# temporal topology for april crimes faceted by years
crime.year.day.time <- dc.april %>%
  group_by(year, dow, hour) %>%
  summarise(COUNT = n())
crime.year.day.time <- subset(crime.year.day.time, !is.na(crime.year.day.time$hour))

ggplot(crime.year.day.time, aes(hour, dow, fill = COUNT)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_y_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) + 
  theme(axis.text.y = element_text(size = 5)) + 
  facet_wrap(~ year, nrow = 6)










