# Step 0
library(tidyverse) # run this every session

# Step 1: Data
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
dc.data$MONTHS <- months(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)
dc.data$HOUR <- substr(dc.data$TIME, 0, 2)
dc.data$WEEK <- format(as.Date(dc.data$DATE), "%U")
dc.data$WEEK <- as.numeric(dc.data$WEEK)

dc.data$TYPE <- case_when(
  dc.data$OFFENSE %in%
    c(
      "ARSON",
      "BURGLARY",
      "MOTOR VEHICLE THEFT",
      "THEFT F/AUTO",
      "THEFT/OTHER") ~ "Property",
  dc.data$OFFENSE %in%
    c("ASSAULT W/DANGEROUS WEAPON", "HOMICIDE", "ROBBERY", 
      "SEX ABUSE")
  ~ "Person")

# Step 2: Graph prep
### Dashed line for 2025-08-11
deployment_week <- as.numeric(format(as.Date("2025-08-11"), "%U"))

### Get weekly counts by Type
crime.weekly <- dc.data %>%
  filter(YEAR >= 2024 & YEAR <= 2025) %>%  # Keep only recent years
  group_by(YEAR, WEEK, TYPE) %>%
  summarise(COUNT = n(), .groups = "drop")

### Get weekly counts by Offense
crime.weekly.offense <- dc.data %>%
  filter(YEAR >= 2024 & YEAR <= 2025) %>% # Keep only recent years
  group_by(YEAR, WEEK, OFFENSE) %>%
  summarise(COUNT = n(), .groups = "drop")

# Step 3: Graphs
### Property vs Person
ggplot(crime.weekly, aes(x = WEEK, y = COUNT, color = as.factor(YEAR), group = YEAR)) +
  geom_line(size = 1) +
  geom_vline(aes(xintercept = deployment_week, linetype = "National Guard"), 
             color = "black", size = 1) +
  scale_linetype_manual("", values = c("National Guard" = "dashed")) +
  facet_wrap(~ TYPE, scales = "free_y") +
  labs(
    title = "Weekly Crime Counts in DC",
    subtitle = "By Crime Category (Property vs Person)",
    x = "Week of Year",
    y = "Number of Incidents",
    color = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 12)
  )

### By Offense
ggplot(crime.weekly.offense, aes(x = WEEK, y = COUNT, color = as.factor(YEAR), group = YEAR)) +
  geom_line(size = 1) +
  geom_vline(aes(xintercept = deployment_week, linetype = "National Guard"), 
             color = "black", size = 1) +
  scale_linetype_manual("", values = c("National Guard" = "dashed")) +
  facet_wrap(~ OFFENSE, scales = "free_y") +
  labs(
    title = "Weekly Crime Counts in DC",
    subtitle = "By Specific Offense",
    x = "Week of Year",
    y = "Number of Incidents",
    color = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 10)
  )
