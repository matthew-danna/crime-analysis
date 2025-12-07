# LIBRARIES
library(tidyverse)
library(zoo)
library(forecast)

# ONLY NEED FOR ARIMA
library(aTSA)
library(tseries)

# GET DATA
calls <- read.csv('/Users/matthewdanna/Downloads/calls 2007-2025 NOV.csv', stringsAsFactors = FALSE)
calls$DATE <- as.Date(calls$date)

# FILTER DATA
### CHOOSE ONLY ONE
### Option 1
calls.subset <- subset(calls, calls$type == 'xxx')

calls.subset <- subset(calls, calls$type == 'xxx' | calls$type == 'yyy')

### Option 2
calls.subset <- subset(calls, calls$lat < 38.87 & calls$lat > 38.86 & calls$lon < -77.278 & 
                         calls$lon > -77.28)

# FORMAT DATA
# Calculate crimes per day:
fairfax.calls <- calls.subset %>%
  group_by(DATE) %>%
  summarise(CALL.COUNT = n())

# Fill in the blank days:
fairfax.calls <- fairfax.calls %>%
  complete(DATE = seq.Date(min(DATE), max(DATE), by = "day")) %>%
  mutate(WEEKDAY = lubridate::wday(DATE, label = T, week_start = 1), 
         MONTH = lubridate::month(DATE, label = T, abbr = F),
         WEEK = isoweek(DATE),
         DAY = day(DATE),
         YEAR = year(DATE))
# replace the NAs with 0s
fairfax.calls$CALL.COUNT <- replace_na(fairfax.calls$CALL.COUNT, 0)

# Change the data type to a time series, update the sequencing:
fairfax.clean <- zoo(fairfax.calls$CALL.COUNT, 
                     seq(from = as.Date(min(fairfax.calls$DATE)), 
                         to = as.Date(max(fairfax.calls$DATE)), by = 1))

# Build a quick graph to understand your data:
autoplot(fairfax.clean) +
  ggtitle("Fairfax Call Type xxx per day: January 2007 to xxxxx 202x") +
  ylab("Calls xxx per day") +
  xlab("Year")

#####
##### ARIMA MODEL CODE
#####
stationary1 <- diff(fairfax.clean, differences = 1)
plot(stationary1)
stationary2 <- diff(fairfax.clean, differences = 2)
plot(stationary2)
stationary3 <- diff(fairfax.clean, differences = 3)
plot(stationary3)

adf.test(as.matrix(fairfax.clean))
adf.test(as.matrix(stationary1))
adf.test(as.matrix(stationary2))
adf.test(as.matrix(stationary3))

# d value equals xxx

pacf(stationaryxxx)
pacf(stationaryxxx, pl=FALSE)

# p value equals xxx

acf(stationaryxxx)
acf(stationaryxxx, pl=FALSE)

# q value equals xxx

arima.data <- auto.arima(fairfax.clean, d = 1, max.p = 12, max.q = 1, seasonal = T)
checkresiduals(arima.data)

forecast.window <- as.numeric(as.Date("2026-12-31")-max(calls.subset$DATE))
forecast.2026 <- forecast::forecast(arima.data, h=forecast.window)

autoplot(forecast.2026)

#####
##### NEURAL NETWORK MODEL CODE
#####

# Build a neural network:
## Option 1: find the best fitting network (takes a long time to run):
fairfax.model <- nnetar(fairfax.clean)

## Option 2: find a network faster, but not nearly as good at predicting:
fairfax.model <- nnetar(y = fairfax.clean, p=1, P=1, size=3, decay=0.1)

### Pick one, and only one of the options above. Then, run the model and get the summary statistics:
fairfax.daily <- forecast::forecast(fairfax.model)
summary(fairfax.daily)

# Build a table of forecasts for 2026:
# Identify the number of days between the last date in your dataset and the end of 2025:
forecast.window <- as.numeric(as.Date("2026-12-31")-max(fairfax.calls$DATE))

# Forecast the number of crimes per day for the rest of 2026, 
# including prediction intervals (depending on the option you chose above, this can take awhile to run):
fairfax.predictions <- forecast::forecast(fairfax.model, PI=TRUE, h=forecast.window)

# Plot the forecast:
autoplot(fairfax.predictions) +
  ggtitle("Fairfax, VA Call Type xxx per day + Forecasts for 2026") +
  ylab("Crime xxx per day")

#####
##### PICK UP ON PROJECT 3 BOTTOM OF PAGE 5
#####

# Extract the forecasted values as a table, clean up the column names, add the forecast date, and month:
forecast.values <- as.data.frame(fairfax.predictions$mean)
forecast.values$ID <- seq.int(nrow(forecast.values))
forecast.upper <- as.data.frame(fairfax.predictions$upper)
forecast.upper$ID <- seq.int(nrow(forecast.upper))
forecast.values <- forecast.values %>%
  left_join(forecast.upper, by = 'ID')
colnames(forecast.values) <- c("MEAN", "ID", "CI80", "CI95")
forecast.values$DATE <- as.Date(max(fairfax.calls$DATE) + forecast.values$ID)
forecast.values$MONTH <- months(forecast.values$DATE)

# Filter to 2026, and summarize forecasts by month:
forecast.values.2026 <- subset(forecast.values, forecast.values$DATE > '2025-xx-xx')
forecast.months <- forecast.values.2026 %>%
  group_by(MONTH) %>%
  summarise(MEAN = round(sum(MEAN),0), FORECAST.95 = round(sum(CI95),0), FORECAST.80 = round(sum(CI80),0))
forecast.months$DIFF <- forecast.months$FORECAST.95 - forecast.months$FORECAST.80

### Visuals
calls.old <- fairfax.calls[c(1,2)]
fairfax.forecast <- forecast.values[c(5,1)]
names(fairfax.forecast) <- c("DATE", "COUNT")
names(calls.old) <- c("DATE", "COUNT")
new.calls <- rbind(calls.old, fairfax.forecast)

graph.new.calls <- ggplot(new.calls, aes(x=DATE, y=COUNT)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  xlab("Years") + 
  ylab("Call Count") + 
  ggtitle("TITLE HERE") + 
  geom_area(fill="lightblue", color="black")
graph.new.calls + 
  geom_smooth(method = lm, col = "red", se = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# new graph
forecast.months$MONTH <- factor(forecast.months$MONTH, levels = forecast.months$MONTH)
forecast.long <- pivot_longer(forecast.months, cols = c(FORECAST.80, DIFF), 
                              names_to = "Category", values_to = "Value")
forecast.long$Category <- gsub("DIFF", "95% Confidence Interval", forecast.long$Category)
forecast.long$Category <- gsub("FORECAST.80", "80% Confidence Interval", forecast.long$Category)


ggplot(forecast.long, aes(x = MONTH, y = Value, fill = fct_rev(Category))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Value), size = 3, colour = 'white', position = position_stack(vjust = 0.5)) + 
  labs(title = "xxx 2026 Monthly Forecast Upper Bounds", 
       x = "Month", 
       y = "Crime Count") +
  scale_fill_manual(values = c("95% Confidence Interval" = "blue",
                               "80% Confidence Interval" = "grey"),
                    name = "Forecasts") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", "July", "August",
                              "September", "October", "November", "December")) + 
  coord_cartesian(ylim = c(0, 300)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# other graph
ggplot(forecast.months, aes(x = MONTH, y = MEAN)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", "July", "August",
                              "September", "October", "November", "December")) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "xxx 2026 Mean Monthly Forecast", 
       x = "Month", 
       y = "Crime Count")


