# packages
library(tidyverse)

# data
arrests <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                          "1YdW0ov1OIm9MNmPLIomfpAy8RO4p6gxG"))
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                          "1X48tHqgfDVTbPTQ4wKxVPFazLyGjOfxz"))
crimes <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                          "1Sg7lkf6EioI87dZiGDsn03un8gxT4zw6"))
crashes <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                          "118xvIlrqOQFsrPVh35CvPmPkNXElenK1"))

# this is probably a bad idea
summary.arrests <- arrests %>%
  group_by(crime.code.description) %>%
  summarise(count = n())

# these only really work with you use all of them
summary.crashes <- crashes %>%
  group_by(type) %>%
  summarise(count = n())

# if you use this, use combinations of crime types
summary.crime <- crimes %>%
  group_by(type) %>%
  summarise(count = n())

summary.calls <- calls %>%
  group_by(type) %>%
  summarise(count = n())

# subsets
sub.arrests <- subset(arrests, 
                      arrests$crime.code.description == 'AGGRAVATED ASSAULT' |
                        arrests$crime.code.description == 'ALL OTHER OFFENSES')

# build forecast model from Project 3 here

# subset my forecast data for all prior June's

# make hotspot maps
### get Fairfax streets and city outline datas

# make temporal topology

