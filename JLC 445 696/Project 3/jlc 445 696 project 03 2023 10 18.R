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

dc.data2023 <- subset(dc.data2023, select = c("CCN", "DATE", "TIME", "SHIFT", "METHOD", "OFFENSE", "BLOCK", "WARD", "ANC", "DISTRICT", "PSA", "NEIGHBORHOOD_CLUSTER", "BLOCK_GROUP", "CENSUS_TRACT", "VOTING_PRECINCT", "LATITUDE", "LONGITUDE", "BID", "HOUR"))
dc.data2023$SHIFT <- toupper(dc.data2023$SHIFT)
dc.data2023$METHOD <- toupper(dc.data2023$METHOD)
dc.data2023$OFFENSE <- toupper(dc.data2023$OFFENSE)
dc.data2023$BLOCK <- toupper(dc.data2023$BLOCK)
dc.data2023$NEIGHBORHOOD_CLUSTER <- toupper(dc.data2023$NEIGHBORHOOD_CLUSTER)
dc.data2023$DATE <- as.Date(dc.data2023$DATE, format = "%m/%d/%Y")

dc.data <- rbind(dc.data.temp, dc.data2023)

dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)

# Step 1.5: subsetting data per shift
dc.day <- subset(dc.data, dc.data$SHIFT == 'DAY')

summary.day <- dc.day %>%
  group_by(OFFENSE) %>%
  summarise(COUNT = n())

# an example, don't choose ARSON
dc.day <- subset(dc.data, dc.data$SHIFT == 'DAY' &
                   dc.data$OFFENSE == 'ROBBERY')

dc.evening <- subset(dc.data, dc.data$SHIFT == 'EVENING')
summary.evening <- dc.evening %>%
  group_by(OFFENSE) %>%
  summarise(COUNT = n())
# an example, don't choose ARSON
dc.evening <- subset(dc.data, dc.data$SHIFT == 'EVENING' &
                       dc.data$OFFENSE == 'ARSON')

dc.midnight <- subset(dc.data, dc.data$SHIFT == 'MIDNIGHT')
summary.midnight <- dc.midnight %>%
  group_by(OFFENSE) %>%
  summarise(COUNT = n())

## Forecasts for subsets of crime
crime.day <- dc.day %>%
  group_by(DATE) %>%
  summarise(COUNT = n())

crime.day <- crime.day %>%
  complete(DATE = seq.Date(min(DATE), max(DATE), by = "day")) %>%
  mutate(WEEKDAY = lubridate::wday(DATE, label = T, week_start = 1), 
         MONTH = lubridate::month(DATE, label = T, abbr = F),
         WEEK = isoweek(DATE),
         DAY = day(DATE),
         YEAR = year(DATE))
crime.day <- replace(crime.day, is.na(crime.day), 0)

cleaned.day <- zoo(crime.day$COUNT, 
                    seq(from = as.Date(min(crime.day$DATE)), 
                        to = as.Date(max(crime.day$DATE)-1), by = 1))
### keep this line below here, but doesn't need to be in your markdown file
summary(cleaned.day)
stationary.day <- diff(cleaned.day)
### keep this line below here, but doesn't need to be in your markdown file
adf.test(as.matrix(stationary.day)) 
arima.day <- auto.arima(stationary.day)

# Daily forecasts
nov1 <- as.Date("2023-11-01")
nov30 <- as.Date("2023-11-30")
start.day <- as.numeric(nov1 - max(crime.day$DATE))
end.day <- as.numeric(nov30 - max(crime.day$DATE))
days.day <- c(start.day:end.day)

forecast00 <- forecast(arima.day, h=(days.day[1]-1))
forecast00 <- round(sum(forecast00$upper[,2]),0)
forecast01 <- forecast(arima.day, h=days.day[1])
forecast01 <- round(sum(forecast01$upper[,2]),0)
forecast02 <- forecast(arima.day, h=days.day[2])
forecast02 <- round(sum(forecast02$upper[,2]),0)
forecast03 <- forecast(arima.day, h=days.day[3])
forecast03 <- round(sum(forecast03$upper[,2]),0)
forecast04 <- forecast(arima.day, h=days.day[4])
forecast04 <- round(sum(forecast04$upper[,2]),0)
forecast05 <- forecast(arima.day, h=days.day[5])
forecast05 <- round(sum(forecast05$upper[,2]),0)
forecast06 <- forecast(arima.day, h=days.day[6])
forecast06 <- round(sum(forecast06$upper[,2]),0)
forecast07 <- forecast(arima.day, h=days.day[7])
forecast07 <- round(sum(forecast07$upper[,2]),0)
forecast08 <- forecast(arima.day, h=days.day[8])
forecast08 <- round(sum(forecast08$upper[,2]),0)
forecast09 <- forecast(arima.day, h=days.day[9])
forecast09 <- round(sum(forecast09$upper[,2]),0)
forecast10 <- forecast(arima.day, h=days.day[10])
forecast10 <- round(sum(forecast10$upper[,2]),0)
forecast11 <- forecast(arima.day, h=days.day[11])
forecast11 <- round(sum(forecast11$upper[,2]),0)
forecast12 <- forecast(arima.day, h=days.day[12])
forecast12 <- round(sum(forecast12$upper[,2]),0)
forecast13 <- forecast(arima.day, h=days.day[13])
forecast13 <- round(sum(forecast13$upper[,2]),0)
forecast14 <- forecast(arima.day, h=days.day[14])
forecast14 <- round(sum(forecast14$upper[,2]),0)
forecast15 <- forecast(arima.day, h=days.day[15])
forecast15 <- round(sum(forecast15$upper[,2]),0)
forecast16 <- forecast(arima.day, h=days.day[16])
forecast16 <- round(sum(forecast16$upper[,2]),0)
forecast17 <- forecast(arima.day, h=days.day[17])
forecast17 <- round(sum(forecast17$upper[,2]),0)
forecast18 <- forecast(arima.day, h=days.day[18])
forecast18 <- round(sum(forecast18$upper[,2]),0)
forecast19 <- forecast(arima.day, h=days.day[19])
forecast19 <- round(sum(forecast19$upper[,2]),0)
forecast20 <- forecast(arima.day, h=days.day[20])
forecast20 <- round(sum(forecast20$upper[,2]),0)
forecast21 <- forecast(arima.day, h=days.day[21])
forecast21 <- round(sum(forecast21$upper[,2]),0)
forecast22 <- forecast(arima.day, h=days.day[22])
forecast22 <- round(sum(forecast22$upper[,2]),0)
forecast23 <- forecast(arima.day, h=days.day[23])
forecast23 <- round(sum(forecast23$upper[,2]),0)
forecast24 <- forecast(arima.day, h=days.day[24])
forecast24 <- round(sum(forecast24$upper[,2]),0)
forecast25 <- forecast(arima.day, h=days.day[25])
forecast25 <- round(sum(forecast25$upper[,2]),0)
forecast26 <- forecast(arima.day, h=days.day[26])
forecast26 <- round(sum(forecast26$upper[,2]),0)
forecast27 <- forecast(arima.day, h=days.day[27])
forecast27 <- round(sum(forecast27$upper[,2]),0)
forecast28 <- forecast(arima.day, h=days.day[28])
forecast28 <- round(sum(forecast28$upper[,2]),0)
forecast29 <- forecast(arima.day, h=days.day[29])
forecast29 <- round(sum(forecast29$upper[,2]),0)
forecast30 <- forecast(arima.day, h=days.day[30])
forecast30 <- round(sum(forecast30$upper[,2]),0)

forecast.day <- c(forecast00, forecast01, forecast02, forecast03, forecast04, 
                   forecast05, forecast06, forecast07, forecast08, forecast09, 
                   forecast10, forecast11, forecast12, forecast13, forecast14, 
                   forecast15, forecast16, forecast17, forecast18, forecast19, 
                   forecast20, forecast21, forecast22, forecast23, forecast24, 
                   forecast25, forecast26, forecast27, forecast28, forecast29, 
                   forecast30)

forecasts.tmp <- data.frame(c(start.day-1,days.day))
colnames(forecasts.tmp) <- "DAY"
forecasts.tmp$ID <- seq.int(nrow(forecasts.tmp))
forecasts.tmp$DATE <- forecasts.tmp$DAY + max(crime.day$DATE)
forecasts.tmp$TOTAL <- forecast.day
forecasts.day <- subset(forecasts.tmp, forecasts.tmp$ID > 1)
forecasts.day$ID <- seq.int(nrow(forecasts.day))
forecasts.tmp <- forecasts.tmp[c(2,4)]
names(forecasts.tmp) <- c("ID", "YESTERDAY")
forecasts.day <- forecasts.day %>% 
  left_join(forecasts.tmp, by = "ID")

forecasts.day$TODAY <- forecasts.day$TOTAL - forecasts.day$YESTERDAY

### BAR GRAPH
ggplot(forecasts.day, aes(DATE, TODAY)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Date",
    y = "Expected Crime Count"
  ) +
  theme(axis.text.x= element_text(angle = 90)) +
  scale_x_continuous("Date", labels = as.character(forecasts.day$DATE), breaks = forecasts.day$DATE)

### HOTSPOT MAP
dc.roads <- roads("DC", "District of Columbia")
dc.roads.major <- dc.roads %>%  filter(RTTYP %in% c("I","S","U"))
dc.outline <- county_subdivisions("DC", "District of Columbia")

dc.november.day <- subset(dc.day, dc.day$MONTH == '11')

ggplot() + 
  geom_sf(data = dc.outline) +
  geom_sf(data = dc.roads.major) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = dc.day, alpha = 0.15, size = 0.5) + 
  theme_void() +
  facet_wrap(~ YEAR, nrow = 4)
  
### TEMPORAL TOPOLOGY
crime.year.day <- dc.november.day %>%
  group_by(YEAR, DOW, HOUR) %>%
  summarise(COUNT = n())
crime.year.day <- subset(crime.year.day, !is.na(crime.year.day$HOUR))

ggplot(crime.year.day, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_y_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  theme(axis.text.y = element_text(size = 10)) + 
  facet_wrap(~ YEAR, nrow = 4)
