# Step 0
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

# Step 1
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

# Step 2
dc.data.temp <- rbind(dc.data2008, dc.data2009, dc.data2010, dc.data2011, 
                      dc.data2012, dc.data2013, dc.data2014, dc.data2015, 
                      dc.data2016, dc.data2017, dc.data2018, dc.data2019, 
                      dc.data2020, dc.data2021, dc.data2022)
dc.data.temp <- separate(dc.data.temp, REPORT_DAT, into = c("DATE", "TIME"), sep = " ")
dc.data.temp <- dc.data.temp %>%
  select(3:9,12:22)
dc.data.temp$DATE <- as.Date(dc.data.temp$DATE, format = "%Y/%m/%d")

dc.data2023 <- separate(dc.data2023.temp, REPORT_DAT, into = c("DATE", "TIME"), sep = ", ")
dc.data2023 <- dc.data2023 %>%
  select(22,26,27,7,28,23,20,10,25,9,16,1,17,3,18,30,4,13)
dc.data2023$SHIFT <- toupper(dc.data2023$SHIFT)
dc.data2023$METHOD <- toupper(dc.data2023$METHOD)
dc.data2023$OFFENSE <- toupper(dc.data2023$OFFENSE)
dc.data2023$BLOCK <- toupper(dc.data2023$BLOCK)
dc.data2023$DATE <- as.Date(dc.data2023$DATE, format = "%m/%d/%Y")

dc.data <- rbind(dc.data.temp, dc.data2023)

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

ggplot(dc.roads) + geom_sf()
ggplot(dc.roads.major) + geom_sf()
ggplot(dc.outline) + geom_sf()

ggplot(dc.data, aes(x=LONGITUDE, y=LATITUDE, color = "red")) + 
  geom_point()

ggplot() +
  geom_sf(data = dc.outline) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color = "red"), data = dc.data) + 
  theme_void()

ggplot() +
  geom_sf(data = dc.outline) + 
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color = "red"), data = dc.data) + 
  geom_sf(data = dc.roads.major) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

ggplot(dc.data, aes(x=LONGITUDE, y=LATITUDE, color = YEAR)) + 
  geom_point()

ggplot(subset(dc.data, dc.data$DOW == 'Friday'), aes(x=LONGITUDE, y=LATITUDE, color = YEAR)) + 
  geom_point()

ggplot(subset(dc.data, dc.data$YEAR == '2023'), 
       aes(x=LONGITUDE, y=LATITUDE, color = DOW)) + 
  geom_point()

# TRANSPARENT POINTS
ggplot() + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = dc.data, alpha = 0.005, size = 0.5) + 
  theme(legend.position="bottom")

# A SUBSET OF DATA WITH TRANSPARENT
data.dc.shift <- subset(dc.data, dc.data$SHIFT == 'MIDNIGHT')
ggplot() + 
  geom_sf(data = dc.outline) +
  geom_sf(data = dc.roads.major) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = data.dc.shift, alpha = 0.005, size = 0.5) + 
  theme(legend.position="bottom") +
  theme_void()

# DENSITY
ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.01),  
                 size = 0.01, bins = 50, data = dc.data, geom = "polygon")

ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.01),  
                 size = 0.01, bins = 5, data = dc.data, geom = "polygon")

ggplot() + 
  geom_sf(data = dc.outline) +
  geom_density2d(data = dc.data, aes(x = LONGITUDE, y = LATITUDE), size = 0.15)

ggplot() + 
  geom_sf(data = dc.outline) +
  geom_density2d(data = subset(dc.data, dc.data$OFFENSE == 'HOMICIDE'), aes(x = LONGITUDE, y = LATITUDE), size = 0.15) +
  ggtitle("Homicide Hotspots, 2008-2023")

ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.01), 
                 size = 0.001, bins = 10, data = dc.data, geom = "polygon") + 
  geom_density2d(data = dc.data, aes(x = LONGITUDE, y = LATITUDE), size = 0.15)

# INTERACTIVE MAP
leaflet(dc.data) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = dc.data$OFFENSE, 
             clusterOptions = markerClusterOptions())

leaflet(dc.data) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = dc.data$OFFENSE, 
             clusterOptions = markerClusterOptions())

leaflet(dc.data) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             popup = paste(
               "Crime Type: ", dc.data$OFFENSE, "<br>",
               "Date:", dc.data$DATE), 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = dc.outline)

dc.data.midnight <- subset(dc.data, dc.data$SHIFT == 'MIDNIGHT')
leaflet(dc.data.midnight) %>%
  addTiles() %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
             popup = paste(
               "Crime Type: ", dc.data.midnight$OFFENSE, "<br>",
               "Date:", dc.data.midnight$DATE), 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = dc.outline)

