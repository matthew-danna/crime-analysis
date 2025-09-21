# Step 0
install.packages('tidyverse')
install.packages('leaflet')
install.packages('sf')
install.packages('tigris')

library(tidyverse)
library(leaflet)
library(sf)
library(tigris)

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

# Step 2
dc.data <- separate(dc.data, REPORT_DAT, into = c("DATE", "TIME"), sep = " ")
dc.data$DATE <- as.Date(dc.data$DATE, format = "%Y/%m/%d")

# Step 3
dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)
dc.data$HOUR <- substr(dc.data$TIME, 0, 2)

# Step 4
dc.roads <- roads("DC", "District of Columbia")
dc.roads.major <- dc.roads %>%  filter(RTTYP %in% c("I","S","U"))
dc.outline <- county_subdivisions("DC", "District of Columbia")
dc.placenames <- landmarks("DC")
dc.water <- area_water("DC", "District of Columbia")
dc.water <- filter(dc.water, AWATER >= 1000)

ggplot(dc.roads) + geom_sf()

ggplot(dc.data, aes(x=LONGITUDE, y=LATITUDE, color = "red")) + 
  geom_point()

ggplot() +
  geom_sf(data = dc.outline) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), color = "red", data = dc.data) + 
  theme_void()

ggplot() +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), color = "red", data = dc.data) + 
  geom_sf(data = dc.roads.major) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

ggplot(dc.data, aes(x=LONGITUDE, y=LATITUDE, color = YEAR)) + 
  geom_point()

ggplot(subset(dc.data, dc.data$DOW == 'Friday'), 
       aes(x=LONGITUDE, y=LATITUDE, color = YEAR)) + 
  geom_point()

ggplot(subset(dc.data, dc.data$YEAR == '2025'), 
       aes(x=LONGITUDE, y=LATITUDE, color = DOW)) + 
  geom_point()

ggplot() + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), 
             data = dc.data, alpha = 0.005, size = 0.5) + 
  theme(legend.position="bottom")

dc1 <- subset(dc.data, dc.data$OFFENSE == 'HOMICIDE')
ggplot() +
  geom_sf(data = dc.outline) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), color = "red", data = dc1, size = 2.0,
             alpha = 0.05) + 
  geom_sf(data = dc.roads.major) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), 
        plot.subtitle = element_text(size = 8, hjust=.5, 
                                     margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

dc2 <- subset(dc.data, dc.data$OFFENSE == 'BURGLARY')

ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.01),  
                 size = 0.01, bins = 10, data = dc2, geom = "polygon")

ggplot() + 
  geom_sf(data = dc.outline) +
  geom_density2d(data = dc2, aes(x = LONGITUDE, y = LATITUDE), size = 0.15)

ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.01), 
                 size = 0.001, bins = 10, data = dc2, geom = "polygon") + 
  geom_density2d(data = dc2, aes(x = LONGITUDE, y = LATITUDE), size = 0.15)

ggplot() +
  geom_hex(aes(x = LONGITUDE, y = LATITUDE), data = dc2, bins = 30) +
  scale_fill_continuous(type = "viridis")

ggplot() + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), 
             data = dc2, alpha = 0.01, size = 0.5) + 
  facet_wrap(~ YEAR, nrow = 4)

ggplot() + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), 
             data = dc2, alpha = 0.01, size = 0.5) + 
  facet_wrap(~ HOUR, nrow = 6)

ggplot() + 
  geom_sf(data = dc.roads, inherit.aes = FALSE, color = "grey", size = .5, alpha = .6) + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE, color = "red"), data = dc.data, alpha = 0.05, size = 0.5) + 
  theme_void() + 
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "With more text here") +
  facet_wrap(~ YEAR, nrow = 5)

ggplot() +
  geom_sf(data = dc.outline$geometry, fill = "transparent", linewidth = 1, 
          color = "black") +
  geom_sf(data = dc.roads$geometry, alpha = 0.1) +
  geom_sf(data = dc.water$geometry, fill = "lightblue") +
  geom_point(data = filter(dc.data, OFFENSE == "HOMICIDE"), 
             aes(x = LONGITUDE, y = LATITUDE), size = 1.0, alpha = 0.15, color = "red") +
  geom_sf_text(data = dc.placenames$geometry, label = dc.placenames$FULLNAME, 
               check_overlap = TRUE, size = 1.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0)),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  labs(title = "A Title", subtitle = "Some other text")

ggplot() +
  geom_sf(data = dc.outline$geometry, fill = "transparent", linewidth = 1, 
          color = "black") +
  geom_sf(data = dc.roads$geometry, alpha = 0.1) +
  geom_sf(data = dc.water$geometry, fill = "lightblue") +
  stat_density2d(data = filter(dc.data, OFFENSE == "BURGLARY"), 
                 aes(x = LONGITUDE, y = LATITUDE, fill = ..level..), 
                 bins = 10, h = 0.01, geom = "polygon", alpha = 0.75) +
  geom_sf_text(data = dc.placenames$geometry, label = dc.placenames$FULLNAME, 
               check_overlap = TRUE, size = 1.5) +
  scale_fill_viridis_c(option = "plasma") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

leaflet(dc.data) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = dc.data$OFFENSE, 
             clusterOptions = markerClusterOptions())

leaflet(dc.data) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = dc.data$OFFENSE, 
             clusterOptions = markerClusterOptions())

dc3 <- subset(dc.data, dc.data$OFFENSE == 'ROBBERY')

leaflet(dc3) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             popup = paste(
               "Crime Type: ", dc3$OFFENSE, "<br>",
               "Date:", dc3$DATE, "<br>",
               "Shift: ", dc3$SHIFT), 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = dc.outline)


