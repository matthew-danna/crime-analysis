# Step 0: Prep
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

# Step 1: Process Data
url <- "https://opendata.dc.gov/datasets/DCGIS::crime-incidents-in-"
years <- c(2008:2024)
full.urls <- paste0(url, years, ".csv")
dc.data <- data.frame()

for(file in full.urls) {
  tmp.data <- read.csv(file, stringsAsFactors = FALSE)
  dc.data <- rbind(tmp.data, dc.data)
}

dc.data <- separate(dc.data, REPORT_DAT, into = c("DATE", "TIME"), 
                    sep = " ")
dc.data$DATE <- as.Date(dc.data$DATE, format = "%Y/%m/%d")

dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$MONTHS <- months(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)
dc.data$HOUR <- substr(dc.data$TIME, 0, 2)

# Step 2: Summarize and filter data
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

##### subsets for modeling
dc1 <- subset(dc.data, dc.data$OFFENSE == 'BURGLARY' & dc.data$SHIFT == 'DAY')
dc2 <- subset(dc.data, dc.data$OFFENSE == 'BURGLARY' & dc.data$SHIFT == 'EVENING')
dc3 <- subset(dc.data, dc.data$OFFENSE == 'BURGLARY' & dc.data$SHIFT == 'MIDNIGHT')

# Step 3: Forecast
### calculate crime per date
crime <- dc.data %>%
  group_by(DATE) %>%
  summarize(COUNT = n())

### fill in blank dates
crime <- crime %>%
  complete(DATE = seq.Date(min(DATE), max(DATE), by = "day")) %>%
  mutate(WEEKDAY = lubridate::wday(DATE, label = T, week_start = 1), 
         MONTH = lubridate::month(DATE, label = T, abbr = F),
         WEEK = isoweek(DATE),
         DAY = day(DATE),
         YEAR = year(DATE))
crime <- replace(crime, is.na(crime), 0)

# change data type, sequence based on last day - 1
cleaned.data <- zoo(crime$COUNT, 
                    seq(from = as.Date(min(crime$DATE)), 
                        to = as.Date(max(crime$DATE)-1), by = 1))

### basic stats
### comparing mean and median in your analysis isn't a bad idea 
summary(cleaned.data)
plot(cleaned.data)
title("Insert A Title Here")

### make data stationary
stationary.data <- diff(cleaned.data)
plot(stationary.data)

### test stationariness
adf.test(as.matrix(stationary.data))

### ARIMA
arima.data <- auto.arima(stationary.data)

### ARIMA to forecast
forecast.7days <- forecast(arima.data, h=7)
additional.7days <- round(sum(forecast.7days$upper[,2]),0)
additional.7days

forecast.1day <- forecast(arima.data, h=1)
additional.1day <- round(sum(forecast.1day$upper[,2]),0)

# Step 4: Visualize
### Crimes per day with a trend line
graph.crime <- ggplot(crime, aes(x=DATE, y=COUNT)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  xlab("Years") + 
  ylab("Crime Count") + 
  ggtitle("Daily Crime in DC") + 
  geom_area(fill="lightblue", color="black")

graph.crime + 
  geom_smooth(method = lm, col = "red", se = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Temporal Topologies
### subset for November
dc.november <- subset(dc.data, dc.data$MONTH == '11')

### group by dow and hour
crime.day.time <- dc.november %>%
  group_by(DOW, HOUR) %>%
  summarise(COUNT = n())
crime.day.time <- subset(crime.day.time, !is.na(crime.day.time$HOUR))

### graphs
ggplot(crime.day.time, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile()

ggplot(crime.day.time, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile() + coord_fixed()

ggplot(crime.day.time, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + 
  coord_fixed()

ggplot(crime.day.time, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = COUNT), color = "white", size = 1.5) +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_fixed()

ggplot(crime.day.time, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = COUNT), color = "black", size = 2.5) +
  scale_fill_gradient(low = "blue", high = "red") +
  guides(fill = guide_colourbar(title = "Crime Count")) +
  coord_fixed()

ggplot(crime.day.time, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = COUNT), color = "black", size = 1.5) +
  scale_fill_gradient(low = "white", high = "red") +
  guides(fill = guide_colourbar(title = "Crime Count", label = FALSE, ticks = FALSE)) +
  coord_fixed()

ggplot(crime.day.time, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = COUNT), color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed()

ggplot(crime.day.time, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile(color = "black", lwd = .5, linetype = 1) + 
  geom_text(aes(label = COUNT), color = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_y_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# faceted per year
crime.year.day.time <- dc.data %>%
  group_by(YEAR, DOW, HOUR) %>%
  summarise(COUNT = n())
crime.year.day.time <- subset(crime.year.day.time, !is.na(crime.year.day.time$HOUR))

ggplot(crime.year.day.time, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_y_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) + 
  theme(axis.text.y = element_text(size = 5)) + 
  facet_wrap(~ YEAR, nrow = 4)




