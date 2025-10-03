# Step 0
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

# Step 1
url <- "https://opendata.dc.gov/datasets/DCGIS::crime-incidents-in-"
years <- c(2008:2025)
full.urls <- paste0(url, years, ".csv")
dc.data <- data.frame()

for(file in full.urls) {
  tmp.data <- read.csv(file, stringsAsFactors = FALSE)
  dc.data <- rbind(tmp.data, dc.data)
}

rm(tmp.data)

dc.data <- separate(dc.data, REPORT_DAT, into = c("DATE", "TIME"), sep = " ")
dc.data$DATE <- as.Date(dc.data$DATE, format = "%Y/%m/%d")

dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)
dc.data$HOUR <- substr(dc.data$TIME, 0, 2)

# Step 2
unique(dc.data$OFFENSE)
unique(dc.data$SHIFT)

summary.offense <- dc.data %>%
  group_by(SHIFT, OFFENSE) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

dc1 <- subset(dc.data, dc.data$OFFENSE == 'xxx' & dc.data$SHIFT == 'DAY')
dc2 <- subset(dc.data, dc.data$OFFENSE == 'xxx' & dc.data$SHIFT == 'EVENING')
dc3 <- subset(dc.data, dc.data$OFFENSE == 'xxx' & dc.data$SHIFT == 'MIDNIGHT')

# Step 3
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
plot(stationary1)
stationary2 <- diff(cleaned.data, differences = 2)
plot(stationary2)
stationary3 <- diff(cleaned.data, differences = 3)
plot(stationary3)

adf.test(as.matrix(cleaned.data))
adf.test(as.matrix(stationary1))
adf.test(as.matrix(stationary2))
adf.test(as.matrix(stationary3)) 

pacf(stationaryxxx)
pacf(stationaryxxx, pl=FALSE)

acf(stationary1)
acf(stationary1, pl=FALSE)

arima.data <- auto.arima(cleaned.data, d = 1, max.p = 6, max.q = 1, seasonal = T)
summary(arima.data)
checkresiduals(arima.data)

forecast.window <- as.numeric(as.Date("2026-12-31")-max(crime$DATE))
forecast.2026 <- forecast(arima.data, h=forecast.window)

autoplot(forecast.2026)

forecast.values <- as.data.frame(forecast.2026$mean)
forecast.values$ID <- seq.int(nrow(forecast.values))
forecast.upper <- as.data.frame(forecast.2026$upper)
forecast.upper$ID <- seq.int(nrow(forecast.upper))
forecast.values <- forecast.values %>%
  left_join(forecast.upper, by = 'ID')
colnames(forecast.values) <- c("MEAN", "ID", "CI80", "CI95")
forecast.values$DATE <- as.Date(max(crime$DATE) + forecast.values$ID)
forecast.values$MONTH <- months(forecast.values$DATE)

forecast.values.2026 <- subset(forecast.values, forecast.values$DATE > '2025-12-31')
forecast.months <- forecast.values.2026 %>%
  group_by(MONTH) %>%
  summarise(MEAN = round(sum(MEAN),0), FORECAST.95 = round(sum(CI95),0), FORECAST.80 = round(sum(CI80),0))
forecast.months$DIFF <- forecast.months$FORECAST.95 - forecast.months$FORECAST.80



crime2025 <- crime[c(1,2)]
crime2026 <- forecast.values[c(5,1)]
names(crime2026) <- c("DATE", "COUNT")
new.crime <- rbind(crime2025, crime2026)

graph.new.crime <- ggplot(new.crime, aes(x=DATE, y=COUNT)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  xlab("Years") + 
  ylab("Crime Count") + 
  ggtitle("TITLE HERE") + 
  geom_area(fill="lightblue", color="black")
graph.new.crime + 
  geom_smooth(method = lm, col = "red", se = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


forecast.months$MONTH <- factor(forecast.months$MONTH, levels = forecast.months$MONTH)
forecast.long <- pivot_longer(forecast.months, cols = c(FORECAST.80, DIFF), 
                              names_to = "Category", values_to = "Value")
forecast.long$Category <- gsub("DIFF", "95% Confidence Interval", forecast.long$Category)
forecast.long$Category <- gsub("FORECAST.80", "80% Confidence Interval", forecast.long$Category)

ggplot(forecast.long, aes(x = MONTH, y = Value, fill = fct_rev(Category))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Value), size = 3, colour = 'white', position = position_stack(vjust = 0.5)) + 
  labs(title = "Washington D.C. 2025 Monthly Forecast Upper Bounds", 
       x = "Month", 
       y = "Crime Count") +
  scale_fill_manual(values = c("95% Confidence Interval" = "blue",
                               "80% Confidence Interval" = "grey"),
                    name = "Forecasts") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", "July", "August",
                              "September", "October", "November", "December")) + 
  coord_cartesian(ylim = c(0, 200)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(forecast.months, aes(x = MONTH, y = MEAN)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", "July", "August",
                              "September", "October", "November", "December")) +
  coord_cartesian(ylim = c(0, 40)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Washington D.C. 2025 Mean Monthly Forecast", 
       x = "Month", 
       y = "Crime Count")


dc1.subset <- subset(dc1, dc1$YEAR >= 2020)

dc.roads <- roads("DC", "District of Columbia")
dc.roads.major <- dc.roads %>%  filter(RTTYP %in% c("I","S","U"))
dc.outline <- county_subdivisions("DC", "District of Columbia")
dc.placenames <- landmarks("DC")
dc.water <- area_water("DC", "District of Columbia")
dc.water <- filter(dc.water, AWATER >= 1000)

ggplot() +
  geom_sf(data = dc.outline$geometry, fill = "transparent", 
          linewidth = 1, color = "black") +
  geom_sf(data = dc.roads$geometry, alpha = 0.1) +
  geom_sf(data = dc.water$geometry, fill = "lightblue") +
  geom_hex(aes(x = LONGITUDE, y = LATITUDE), data = dc1.subset, bins = 12,
           alpha = 0.5) +
  scale_fill_continuous(type = "viridis") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  facet_wrap(~MONTH)

crime.day.time <- dc1.subset %>%
  group_by(DOW, HOUR) %>%
  summarise(COUNT = n())
crime.day.time <- replace(crime.day.time, is.na(crime.day.time), 0)

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

crime.year.day.time <- dc1.subset %>%
  group_by(YEAR, MONTH, DOW, HOUR) %>%
  summarise(COUNT = n())
crime.year.day.time <- replace(crime.year.day.time, is.na(crime.year.day.time), 0)

ggplot(crime.year.day.time, aes(HOUR, DOW, fill = COUNT)) +
  geom_tile() + 
  scale_fill_gradient(low = "#a0b7db", high = "red") +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_y_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) + 
  theme(axis.text.y = element_text(size = 5)) + 
  facet_wrap(~ MONTH)


