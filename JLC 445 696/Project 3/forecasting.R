library(tidyverse)
library(leaflet)
library(sf)
library(tigris)
library(data.table)
library(zoo)
library(aTSA)
library(tseries)
library(forecast)

### PROCESS DATA
# get data
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

# clean 2023
dc.data2023 <- separate(dc.data2023.temp, REPORT_DAT, into = c("DATE", "TIME"), sep = ", ")
dc.data2023 <- subset(dc.data2023, select = c("CCN", "DATE", "TIME", "SHIFT", "METHOD", "OFFENSE", "BLOCK", "WARD", 
                                              "ANC", "DISTRICT", "PSA", "NEIGHBORHOOD_CLUSTER", "BLOCK_GROUP", 
                                              "CENSUS_TRACT", "VOTING_PRECINCT", "LATITUDE", "LONGITUDE", "BID"))
dc.data2023$SHIFT <- toupper(dc.data2023$SHIFT)
dc.data2023$METHOD <- toupper(dc.data2023$METHOD)
dc.data2023$OFFENSE <- toupper(dc.data2023$OFFENSE)
dc.data2023$BLOCK <- toupper(dc.data2023$BLOCK)
dc.data2023$NEIGHBORHOOD_CLUSTER <- toupper(dc.data2023$NEIGHBORHOOD_CLUSTER)
dc.data2023$DATE <- as.Date(dc.data2023$DATE, format = "%m/%d/%Y")

# merge
dc.data <- rbind(dc.data.temp, dc.data2023)

# enrich
dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)
dc.data$HOUR <- substr(dc.data$TIME, 0, 2)

### FIND AN OFFENSE, SHIFT, AND DISTRICT
unique(dc.data$OFFENSE)
unique(dc.data$METHOD)
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

summary.offense.shift <- dc.data %>%
  group_by(OFFENSE, SHIFT) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

summary.district <- dc.data %>%
  group_by(DISTRICT) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

summary.offense.district <- dc.data %>%
  group_by(OFFENSE, DISTRICT) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

dc.data.full <- subset(dc.data, !is.na(dc.data$DISTRICT))
dc.data.missing <- subset(dc.data, is.na(dc.data$DISTRICT))

data.robbery <- subset(dc.data, dc.data$OFFENSE == 'ROBBERY')
data.gun <- subset(dc.data, dc.data$METHOD == 'GUN')
data.midnight <- subset(dc.data, dc.data$SHIFT == 'MIDNIGHT')
data.seven <- subset(dc.data, dc.data$DISTRICT == '7')

dc1 <- subset(dc.data, dc.data$OFFENSE == 'HOMICIDE' & dc.data$SHIFT == 'MIDNIGHT')
dc2 <- subset(dc.data, dc.data$OFFENSE == 'ROBBERY' & dc.data$SHIFT == 'EVENING')
dc3 <- subset(dc.data, dc.data$OFFENSE == 'HOMICIDE' & dc.data$SHIFT == 'MIDNIGHT')

### PREPARE MODEL
# crimes per day
crime.evening <- dc1 %>%
  group_by(DATE) %>%
  summarise(COUNT = n())

# fill in the blank days
crime.evening <- crime.evening %>%
  complete(DATE = seq.Date(min(DATE), max(DATE), by = "day")) %>%
  mutate(WEEKDAY = lubridate::wday(DATE, label = T, week_start = 1), 
         MONTH = lubridate::month(DATE, label = T, abbr = F),
         WEEK = isoweek(DATE),
         DAY = day(DATE),
         YEAR = year(DATE))
crime.evening <- replace(crime.evening, is.na(crime.evening), 0)

# clean and standardize: 
cleaned.evening <- zoo(crime.evening$COUNT, 
                       seq(from = as.Date(min(crime.evening$DATE)), 
                           to = as.Date(max(crime.evening$DATE)-1), by = 1))

summary(cleaned.evening)
plot(cleaned.evening)
stationary.evening <- diff(cleaned.evening)

# test for stationariness:
adf.test(as.matrix(stationary.evening))

# build model:
arima.evening <- auto.arima(stationary.evening)

# forecast:
forecast.7days <- forecast(arima.evening, h=7)
additional.7days <- round(sum(forecast.7days$upper[,2]),0)
additional.7days

# forecast november:
nov1 <- as.Date("2023-11-01")
nov30 <- as.Date("2023-11-30")
start <- as.numeric(nov1 - max(crime.evening$DATE))
end <- as.numeric(nov30 - max(crime.evening$DATE))

days <- c(start:end)

forecast00 <- forecast(arima.evening, h=(days[1]-1))
forecast00 <- round(sum(forecast00$upper[,2]),0)
forecast01 <- forecast(arima.evening, h=days[1])
forecast01 <- round(sum(forecast01$upper[,2]),0)
forecast02 <- forecast(arima.evening, h=days[2])
forecast02 <- round(sum(forecast02$upper[,2]),0)
forecast03 <- forecast(arima.evening, h=days[3])
forecast03 <- round(sum(forecast03$upper[,2]),0)
forecast04 <- forecast(arima.evening, h=days[4])
forecast04 <- round(sum(forecast04$upper[,2]),0)
forecast05 <- forecast(arima.evening, h=days[5])
forecast05 <- round(sum(forecast05$upper[,2]),0)
forecast06 <- forecast(arima.evening, h=days[6])
forecast06 <- round(sum(forecast06$upper[,2]),0)
forecast07 <- forecast(arima.evening, h=days[7])
forecast07 <- round(sum(forecast07$upper[,2]),0)
forecast08 <- forecast(arima.evening, h=days[8])
forecast08 <- round(sum(forecast08$upper[,2]),0)
forecast09 <- forecast(arima.evening, h=days[9])
forecast09 <- round(sum(forecast09$upper[,2]),0)
forecast10 <- forecast(arima.evening, h=days[10])
forecast10 <- round(sum(forecast10$upper[,2]),0)
forecast11 <- forecast(arima.evening, h=days[11])
forecast11 <- round(sum(forecast11$upper[,2]),0)
forecast12 <- forecast(arima.evening, h=days[12])
forecast12 <- round(sum(forecast12$upper[,2]),0)
forecast13 <- forecast(arima.evening, h=days[13])
forecast13 <- round(sum(forecast13$upper[,2]),0)
forecast14 <- forecast(arima.evening, h=days[14])
forecast14 <- round(sum(forecast14$upper[,2]),0)
forecast15 <- forecast(arima.evening, h=days[15])
forecast15 <- round(sum(forecast15$upper[,2]),0)
forecast16 <- forecast(arima.evening, h=days[16])
forecast16 <- round(sum(forecast16$upper[,2]),0)
forecast17 <- forecast(arima.evening, h=days[17])
forecast17 <- round(sum(forecast17$upper[,2]),0)
forecast18 <- forecast(arima.evening, h=days[18])
forecast18 <- round(sum(forecast18$upper[,2]),0)
forecast19 <- forecast(arima.evening, h=days[19])
forecast19 <- round(sum(forecast19$upper[,2]),0)
forecast20 <- forecast(arima.evening, h=days[20])
forecast20 <- round(sum(forecast20$upper[,2]),0)
forecast21 <- forecast(arima.evening, h=days[21])
forecast21 <- round(sum(forecast21$upper[,2]),0)
forecast22 <- forecast(arima.evening, h=days[22])
forecast22 <- round(sum(forecast22$upper[,2]),0)
forecast23 <- forecast(arima.evening, h=days[23])
forecast23 <- round(sum(forecast23$upper[,2]),0)
forecast24 <- forecast(arima.evening, h=days[24])
forecast24 <- round(sum(forecast24$upper[,2]),0)
forecast25 <- forecast(arima.evening, h=days[25])
forecast25 <- round(sum(forecast25$upper[,2]),0)
forecast26 <- forecast(arima.evening, h=days[26])
forecast26 <- round(sum(forecast26$upper[,2]),0)
forecast27 <- forecast(arima.evening, h=days[27])
forecast27 <- round(sum(forecast27$upper[,2]),0)
forecast28 <- forecast(arima.evening, h=days[28])
forecast28 <- round(sum(forecast28$upper[,2]),0)
forecast29 <- forecast(arima.evening, h=days[29])
forecast29 <- round(sum(forecast29$upper[,2]),0)
forecast30 <- forecast(arima.evening, h=days[30])
forecast30 <- round(sum(forecast30$upper[,2]),0)

forecast.list <- c(forecast00, forecast01, forecast02, forecast03, forecast04, forecast05, forecast06,
                   forecast07, forecast08, forecast09, forecast10, forecast11, forecast12, forecast13, forecast14,
                   forecast15, forecast16, forecast17, forecast18, forecast19, forecast20, forecast21, forecast22,
                   forecast23, forecast24, forecast25, forecast26, forecast27, forecast28, forecast29, forecast30)

forecasts.tmp <- data.frame(c(start-1,days))
colnames(forecasts.tmp) <- "DAYS"
forecasts.tmp$ID <- seq.int(nrow(forecasts.tmp))
forecasts.tmp$DATE <- forecasts.tmp$DAYS + max(crime.evening$DATE)
forecasts.tmp$TOTAL <- forecast.list
forecasts <- subset(forecasts.tmp, forecasts.tmp$ID > 1)
forecasts$ID <- seq.int(nrow(forecasts))
forecasts.tmp <- forecasts.tmp[c(2,4)]
names(forecasts.tmp) <- c("ID", "YESTERDAY")
forecasts <- forecasts %>% left_join(forecasts.tmp, by = "ID")

forecasts$TODAY <- forecasts$TOTAL - forecasts$YESTERDAY

### Visuals
# graph of crime per day with a trend line
graph.evening <- ggplot(crime.evening, aes(x=DATE, y=COUNT)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  xlab("Months") + 
  ylab("Crime") + 
  ggtitle("TITLE HERE") + 
  geom_area(fill="lightblue", color="black")

graph.evening + 
  geom_smooth(method = lm, col = "red", se = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# forecast
total.7days <- round(sum(crime.evening$count) + additional.7days, 0)
plot(forecast.7days)

##### Add maps??

###### Add temporal topology

