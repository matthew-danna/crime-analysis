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
dc.data2024 <- read.csv("https://opendata.arcgis.com/datasets/c5a9f33ffca546babbd91de1969e742d_6.csv", stringsAsFactors = FALSE)
dc.data2023 <- read.csv("https://opendata.arcgis.com/datasets/89561a4f02ba46cca3c42333425d1b87_5.csv", stringsAsFactors = FALSE)
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

# Step 2
dc.data.temp <- rbind(dc.data2010, dc.data2011, dc.data2012, dc.data2013,
                      dc.data2014, dc.data2015, dc.data2016, dc.data2017,
                      dc.data2018, dc.data2019, dc.data2020, dc.data2021,
                      dc.data2022, dc.data2023, dc.data2024)
dc.data <- separate(dc.data.temp, REPORT_DAT, into = c("DATE", "TIME"), sep = " ")

dc.data$DATE <- as.Date(dc.data$DATE, format = "%Y/%m/%d")

# Step 3
dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)
dc.data$HOUR <- substr(dc.data$TIME, 0, 2)

# Step 4
dc.data.full <- subset(dc.data, !is.na(dc.data$LATITUDE))

dc.roads <- roads("DC", "District of Columbia")
dc.roads.major <- dc.roads %>%  filter(RTTYP %in% c("I","S","U"))
dc.outline <- county_subdivisions("DC", "District of Columbia")

ggplot(dc.roads) +
  geom_sf()
ggplot(dc.roads.major) +
  geom_sf()

ggplot(dc.data.full, aes(x=LONGITUDE, y=LATITUDE, color = "red")) +
  geom_point()

dc.subset <- subset(dc.data.full, dc.data.full$YEAR == '2024')

ggplot() +
  geom_sf(data = dc.outline) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color = "red"), data = dc.subset) + 
  theme_void()

ggplot() +
  geom_sf(data = dc.outline) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color = "red"), data = dc.subset) + 
  geom_sf(data = dc.roads.major) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

ggplot(dc.subset, aes(x=LONGITUDE, y=LATITUDE, color = DOW)) + 
  geom_point()

ggplot(subset(dc.subset, dc.data$OFFENSE == 'BURGLARY'), 
       aes(x=LONGITUDE, y=LATITUDE, color = SHIFT)) + 
  geom_point()

# Transparency - focus on the alpha
ggplot() + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = dc.subset, 
             alpha = 0.20, size = 2.5) + 
  theme(legend.position="bottom")

# Density - focus on the bins
ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.01),  
                 size = 0.01, bins = 5, data = dc.subset, geom = "polygon")

ggplot() + 
  geom_sf(data = dc.outline) +
  geom_density2d(data = dc.subset, aes(x = LONGITUDE, y = LATITUDE), 
                 size = 0.15) +
  ggtitle("Crime Hotspots, 2024")

ggplot() + 
  geom_sf(data = dc.outline) +
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.01),  
                 size = 0.01, bins = 5, data = dc.subset, geom = "polygon") +
  ggtitle("Crime Hotspots, 2024") +
  theme_void()

ggplot() + 
  geom_sf(data = dc.outline) + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = dc.subset, 
             alpha = 0.2, size = 1.25) + 
  theme_void() +
  facet_wrap(~ OFFENSE, nrow = 4)

# Interactive
leaflet(dc.subset) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = dc.subset$OFFENSE, 
             clusterOptions = markerClusterOptions())

leaflet(dc.subset) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             popup = paste(
               "Crime Type: ", dc.subset$OFFENSE, "<br>",
               "Date:", dc.subset$DATE), 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = dc.outline)


