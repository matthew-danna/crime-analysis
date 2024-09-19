# Step 0: libraries
install.packages('tidyverse')
install.packages('leaflet')
install.packages('sf')
install.packages('tigris')
install.packages('data.table')

library(tidyverse)
library(leaflet)
library(sf)
library(tigris)
library(data.table)

# Step 1: getting the data
url <- "https://opendata.dc.gov/datasets/DCGIS::crime-incidents-in-"
years <- c(2008:2024)
full.urls <- paste0(url, years, ".csv")

dc.data <- data.frame()

for(file in full.urls) {
  tmp.data <- read.csv(file, stringsAsFactors = FALSE)
  dc.data <- rbind(tmp.data, dc.data)
}

# Step 2: cleaning data
dc.data <- separate(dc.data, REPORT_DAT, into = c("DATE", "TIME"),
                    sep = " ")
dc.data$DATE <- as.Date(dc.data$DATE, format = "%Y/%m/%d")

# Step 3: enriching data
dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$MONTHS <- months(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)
dc.data$HOUR <- substr(dc.data$TIME, 0, 2)

# Step 4: maps!
dc.data.full <- subset(dc.data, !is.na(dc.data$LATITUDE))

dc.roads <- roads("DC","District of Columbia")
dc.roads.major <- dc.roads %>% filter(RTTYP %in% c("I", "S", "U"))
dc.outline <- county_subdivisions("DC", "District of Columbia")  
  
ggplot(dc.roads) + geom_sf()

dc.subset <- subset(dc.data.full, 
                    dc.data.full$OFFENSE == 'HOMICIDE')

ggplot(dc.subset, aes(x=LONGITUDE, y=LATITUDE, color = "red")) +
  geom_point()

ggplot() +
  geom_sf(data = dc.outline) +
  geom_sf(data = dc.roads) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color = "red"), 
             data = dc.subset) + 
  theme_void() + 
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

ggplot(dc.subset, aes(x=LONGITUDE, y=LATITUDE, color = YEAR)) + 
  geom_point()

ggplot(subset(dc.subset, dc.subset$DOW == 'Friday'), 
       aes(x=LONGITUDE, y=LATITUDE, color = YEAR)) + 
  geom_point()

ggplot(subset(dc.subset, dc.subset$YEAR == '2024'), 
       aes(x=LONGITUDE, y=LATITUDE, color = DOW)) + 
  geom_point()

# TRANSPARENT POINTS
ggplot() + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = dc.data, 
             alpha = 0.005, size = 0.5) + 
  theme(legend.position="bottom")

ggplot() + 
  geom_sf(data = dc.outline) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = dc.subset, 
             alpha = 0.2, size = 0.5, color = "blue") + 
  geom_sf(data = dc.roads.major) +
  theme(legend.position="bottom") +
  theme_void() + 
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "DC Homicides", 
       subtitle = "January 2008- September 2024")

# DENSITY
ggplot() + 
  geom_sf(data = dc.outline) +
  geom_sf(data = dc.roads.major) +
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, 
                     fill = ..level.., alpha = 0.01),  
                 size = 0.01, bins = 8, data = dc.subset, 
                 geom = "polygon") +
  theme_void() + 
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "DC Homicides", 
       subtitle = "January 2008- September 2024")

# CONTOUR
ggplot() + 
  geom_density2d(data = dc.subset, 
                 aes(x = LONGITUDE, y = LATITUDE), size = 0.15)

# DENSITY + CONTOUR
ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, 
                     fill = ..level.., alpha = 0.01), 
                 size = 0.001, bins = 10, data = dc.subset, 
                 geom = "polygon") + 
  geom_density2d(data = dc.subset, 
                 aes(x = LONGITUDE, y = LATITUDE), size = 0.15)

# HEXAGON
ggplot() +
  geom_sf(data = dc.outline) +
  geom_hex(aes(x = LONGITUDE, y = LATITUDE), data = dc.subset, 
           bins = 20) +
  scale_fill_continuous(type = "viridis") +
  geom_sf(data = dc.roads.major) +
  theme_void() + 
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "DC Homicides", 
       subtitle = "January 2008- September 2024") +
  facet_wrap(~YEAR)
  
# INTERACTIVE!
leaflet(dc.subset) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             popup = dc.subset$DATE, 
             clusterOptions = markerClusterOptions())

leaflet(dc.subset) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             popup = paste(
               "Crime Type: ", dc.subset$OFFENSE, "<br>",
               "Date:", dc.subset$DATE), 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = dc.outline)




