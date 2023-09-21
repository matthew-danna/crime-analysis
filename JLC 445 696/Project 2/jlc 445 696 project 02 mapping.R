# Step 0
#install.packages('tidyverse')
#install.packages('leaflet')
#install.packages('sf')
#install.packages('tigris')
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)

# Step 1
dc.data <- read.csv("https://opendata.arcgis.com/datasets/89561a4f02ba46cca3c42333425d1b87_5.csv", stringsAsFactors = FALSE) 
dc.data <- separate(dc.data, REPORT_DAT, into = c("date", "time"), sep = " ")
dc.data$date <- as.Date(dc.data$date, format = "%Y/%m/%d")
dc.data$year <- substr(dc.data$date, 0, 4)
dc.data$month <- month(dc.data$date)
dc.data$day <- day(dc.data$date)
dc.data$dow <- weekdays(dc.data$date)
dc.data$hour <- substr(dc.data$time, 0, 2)

# Geographic data
dc.roads <- roads("DC", "District of Columbia")
dc.roads.major <- dc.roads %>%
  filter(RTTYP %in% c("I","S","U"))
dc <- county_subdivisions("DC", "District of Columbia")

# get rid of blanks
dc.data.full <- subset(dc.data, !is.na(dc.data$LATITUDE))

ggplot() +
  geom_sf(data = dc) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color = "red"), data = dc.data) +   
  geom_sf(data = dc.roads.major)


# Step 3
calls.year <- calls.full %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

calls.type <- calls.full %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

calls.type <- calls.type[order(-calls.type$count),] # sort in descending order by count
diffs <- diff(calls.type$PCT)
diffs <- c(0, diffs)
calls.type$diff <- diffs

# Step 4 - JUST CHOOSE ONE
# for a single call type:
calls <- subset(calls.full, calls.full$type == 'MISC')
# for multiple call types:
calls <- subset(calls.full, calls.full$type == 'SHOOTING' | calls.full$type == 'STABBING' | calls.full$type == 'ARSON')

# Step 5
## points, roads, outline, and titles
ggplot() +
  geom_sf(data = dc) +
  geom_point(aes(x=lon, y=lat, color = "red"), data = calls) + 
  geom_sf(data = dc.roads) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "Fairfax, VA", subtitle = "Calls on the streets with the city outline")

# transparent points - UPDATE THE ALPHA AND SIZE!
ggplot() +
  geom_point(aes(x=lon, y=lat), data = calls, alpha = 0.10, size = 1.0) +
  theme(legend.position = "bottom")

# densities and contours
ggplot() + 
  geom_sf(data = fairfax.city) +
  geom_sf(data = fairfax.roads) +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = 0.01), 
                 size = 0.001, bins = 5, data = calls, geom = "polygon") + 
  geom_density2d(data = calls, aes(x = lon, y = lat), size = 0.15) +
  theme_bw()

ggplot() + 
  geom_sf(data = fairfax.city) +
  geom_sf(data = fairfax.roads) +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = 0.01), 
                 size = 0.001, bins = 5, data = calls, geom = "polygon") + 
  theme_classic() + 
  theme(legend.position = "none") +
  facet_wrap(~ year, nrow = 3)

# transparent points per year, with streets, titles, and a clean background:
ggplot() + 
  geom_sf(data = fairfax.roads, inherit.aes = FALSE, color = "grey", size = .3, alpha = .5) + 
  geom_point(aes(x = lon, y = lat, color = "red"), data = calls, alpha = 0.1, size = 1.5) +  
  theme_void() + 
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "Fairfax, VA", subtitle = "Calls and streets") +
  facet_wrap(~ year, nrow = 3) +
  theme(legend.position = "none")


leaflet(subset(dc.data, dc.data$OFFENSE == 'ROBBERY')) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             clusterOptions = markerClusterOptions())


# interactive clusters - city boundary and more pop-up text
leaflet(dc.data) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             popup = paste(
               "Crime Type: ", dc.data$OFFENSE, "<br>",
               "Date:", dc.data$date), 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = dc)




