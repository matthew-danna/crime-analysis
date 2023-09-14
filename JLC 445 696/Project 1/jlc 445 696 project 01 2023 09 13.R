# Step 0: Load packages
install.packages('tidyverse')
library(tidyverse)

# Step 1: Get Data
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
dc.data2009 <- read.csv("https://opendata.arcgis.com/datasets/73cd2f2858714cd1a7e2859f8e6e4de4_33.csv", stringsAsFactors = FALSE)
dc.data2008 <- read.csv("https://opendata.arcgis.com/datasets/180d56a1551c4e76ac2175e63dc0dce9_32.csv", stringsAsFactors = FALSE)

# Step 2: Clean data
dc.data.temp <- rbind(dc.data2008, dc.data2009, dc.data2010, dc.data2011, dc.data2012, dc.data2013, dc.data2014, dc.data2015, dc.data2016, dc.data2017, dc.data2018, dc.data2019, dc.data2020, dc.data2021, dc.data2022, dc.data2023)
dc.data <- separate(dc.data.temp, REPORT_DAT, into = c("date", "time"), sep = " ")
dc.data$date <- as.Date(dc.data$date, format = "%Y/%m/%d")

# Step 3: Enrich data
dc.data$year <- substr(dc.data$date, 0, 4)
dc.data$month <- month(dc.data$date)
dc.data$day <- day(dc.data$date)
dc.data$dow <- weekdays(dc.data$date)
dc.data$hour <- substr(dc.data$time, 0, 2)

# Step 4: Graphs!
ggplot() # creates an empty object
ggplot(dc.data, aes(SHIFT)) # points to data and specifies x axis
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count") # bar graph
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE))
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE), position = position_stack(reverse = TRUE))
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE), position = position_stack(reverse = TRUE)) + theme_classic()
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE), position = position_stack(reverse = TRUE)) + theme_classic() + theme(legend.position = "bottom")
ggplot(dc.data, aes(SHIFT)) + 
  geom_bar(stat = "count", aes(fill = OFFENSE), 
           position = position_stack(reverse = TRUE)) + 
  theme_classic() + 
  facet_wrap(~DISTRICT)

# make one of the graphs an object:
graph1 <- ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count")

graph1 + labs(
  title = "Graph 1: Crimes per Shift",
  subtitle = "My name",
  x = "Shift",
  y = "Number of crimes"
)

# repeat for graphs 2 and 3
graph2 <- ggplot(dc.data, aes('SOMETHING ELSE')) + geom_bar(stat = "count")

graph2 + labs(
  title = "Graph 2: aaaaaaaaaaa",
  subtitle = "My name",
  x = "xxxxxxxx",
  y = "yyyyyyyy"
)


