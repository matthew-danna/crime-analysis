# STEP 0
#install.packages('tidyverse')
#install.packages('leaflet')
#install.packages('sf')
#install.packages('tigris')
#install.packages('data.table')
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)
library(data.table)

# STEP 1
url <- "https://opendata.dc.gov/datasets/DCGIS::crime-incidents-in-"
years <- c(2008:2025)
full.urls <- paste0(url, years, ".csv")
dc.data <- data.frame()

for(file in full.urls) {
  tmp.data <- read.csv(file, stringsAsFactors = FALSE)
  dc.data <- rbind(tmp.data, dc.data)
}

# STEP 2
dc.data <- separate(dc.data, REPORT_DAT, into = c("DATE", "TIME"), sep = " ")
dc.data$DATE <- as.Date(dc.data$DATE, format = "%Y/%m/%d")

# STEP 3
dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$MONTHS <- months(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)
dc.data$HOUR <- substr(dc.data$TIME, 0, 2)

# STEP 4
dc.data.full <- subset(dc.data, !is.na(dc.data$LATITUDE))

dc.roads <- roads("DC", "District of Columbia")
dc.outline <- county_subdivisions("DC", "District of Columbia")

ggplot(dc.data.full, aes(x=LONGITUDE, y=LATITUDE, color = "red")) + 
  geom_point()

ggplot() +
#  geom_sf(data = dc.outline) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), color = "blue", data = dc.data) + 
  theme_void()

ggplot() +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), color = "red", data = dc.data) + 
#  geom_sf(data = dc.roads.major) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), 
        plot.subtitle = element_text(size = 8, hjust=.5, 
                                     margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

ggplot(dc.data.full, aes(x=LONGITUDE, y=LATITUDE, color = YEAR)) + 
  geom_point()

ggplot(subset(dc.data.full, dc.data.full$DOW == 'Friday'), 
       aes(x=LONGITUDE, y=LATITUDE, color = YEAR)) + geom_point()

# TRANSPARENT POINTS
ggplot() + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = dc.data.full, 
             alpha = 0.005, size = 0.5) + 
  theme(legend.position="bottom")


dc.map1 <- subset(dc.data.full, dc.data.full$OFFENSE == 'ROBBERY')
ggplot() + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = dc.map1, 
             alpha = 0.05, size = 0.5) + theme_classic()

# DENSITY MAP
ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., 
                     alpha = 0.01),  
                 size = 0.01, bins = 50, data = dc.data.full, 
                 geom = "polygon")

ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., 
                     alpha = 0.01),  
                 size = 0.01, bins = 7, data = dc.data.full, 
                 geom = "polygon")

# CONTOUR MAP
ggplot() + 
#  geom_sf(data = dc.outline) +
  geom_density2d(data = dc.data.full, aes(x = LONGITUDE, y = LATITUDE), 
                 size = 0.15)

# HEX MAP
ggplot() +
  geom_hex(aes(x = LONGITUDE, y = LATITUDE), data = dc.data.full, 
           bins = 25) +
  scale_fill_continuous(type = "viridis")

ggplot() +
  geom_hex(aes(x = LONGITUDE, y = LATITUDE), data = dc.data.full, 
           bins = 25) +
  scale_fill_continuous(type = "viridis") +
  facet_wrap(~YEAR)

ggplot() +
#  geom_sf(data = dc.outline$geometry, fill = "transparent", linewidth = 1, color = "black") +
#  geom_sf(data = dc.roads.major$geometry) +
#  geom_sf(data = dc.water$geometry, fill = "lightblue") +
  stat_density2d(data = filter(dc.data.full, OFFENSE == "ROBBERY"), aes(x = LONGITUDE, y = LATITUDE, fill = ..level..), bins = 10, h = 0.01, geom = "polygon", alpha = 0.75) +
#  geom_sf_text(data = dc.placenames$geometry, label = dc.placenames$FULLNAME, check_overlap = TRUE, size = 0.5) +
  scale_fill_viridis_c(option = "plasma") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

# INTERACTIVE MAP
leaflet(dc.data.full) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = dc.data.full$OFFENSE, 
             clusterOptions = markerClusterOptions())




