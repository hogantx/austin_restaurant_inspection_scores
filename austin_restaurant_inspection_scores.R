setwd("C:/Users/bhogan/Documents/R/Data")
library(ggplot2)
library(scales)
library(xts)
library(zoo)
library(tseries)
library(reshape)
library(forecast)
library(rattle)
library(manipulate)
library(googleVis)
library(ggthemes)
library(lattice)
library(dplyr)
library(tm)

https://data.austintexas.gov/api/views/ecmv-9xxi/rows.csv?accessType=DOWNLOAD

fileUrl <- "http://data.austintexas.gov/api/views/ecmv-9xxi/rows.csv?accessType=DOWNLOAD"
	message("Downloading data")
	download.file(fileUrl, destfile = "austin_restaurant.csv")

https://data.austintexas.gov/api/views/ecmv-9xxi/rows.csv?accessType=DOWNLOAD


aus_rest <- read.csv("austin_restaurant.csv")
head(aus_rest)
names(aus_rest) <- tolower(names(aus_rest))

top_30 <- c("Subway", "HEB", "7 Eleven",
"Starbucks", "Randall's", "McDonalds", 
"Corner Store", "Sonic Drive In", "Target", 
"Wal-Mart", "ABIA", "Jack in the Box", 
"Thundercloud Subs", "Tokyo Garden", "Whataburger", 
"Whole Foods", "Sam's Club", "Pizza Hut", 
"Taco Cabana", "Taco Bell", "Mr. Gatti's", 
"Papa John's", "Chipotle Mexican Grill", "Amy's Ice Cream", 
"Austin's Pizza", "Jimmy Johns", "Panda Express", 
"Wendy's", "Little Caesars", "Taco Shack")


aus_rest$restaurant.name <- as.character(aus_rest$restaurant.name)
aus_rest$restaurant <- aus_rest$restaurant.name

for (i in 1:30) {
  aus_rest$restaurant <- ifelse(
    agrepl(top_30[i], aus_rest$restaurant, max.distance = 0.1), 
    top_30[i],
    aus_rest$restaurant)
  }

aus_rest_2 <- aus_rest %>%
  filter(aus_rest$restaurant == "Subway" |
  aus_rest$restaurant == "HEB" |
  aus_rest$restaurant == "7 Eleven" |
  aus_rest$restaurant == "Starbucks" |
  aus_rest$restaurant == "Randall's" |
  aus_rest$restaurant == "McDonalds" |
  aus_rest$restaurant == "Corner Store" |
  aus_rest$restaurant == "Sonic Drive In" |
  aus_rest$restaurant == "Target" |
  aus_rest$restaurant == "Wal-Mart" |
  aus_rest$restaurant == "ABIA" |
  aus_rest$restaurant == "Jack in the Box" |
  aus_rest$restaurant == "Thundercloud Subs" |
  aus_rest$restaurant == "Tokyo Garden" |
  aus_rest$restaurant == "Whataburger" |
  aus_rest$restaurant == "Whole Foods" |
  aus_rest$restaurant == "Sam's Club" |
  aus_rest$restaurant == "Pizza Hut" |
  aus_rest$restaurant == "Taco Cabana" |
  aus_rest$restaurant == "Taco Bell" |
  aus_rest$restaurant == "Mr. Gatti's" |
  aus_rest$restaurant == "Papa John's" |
  aus_rest$restaurant == "Chipotle Mexican Grill" |
  aus_rest$restaurant == "Amy's Ice Cream" |
  aus_rest$restaurant == "Austin's Pizza" |
  aus_rest$restaurant == "Jimmy Johns" |
  aus_rest$restaurant == "Panda Express" |
  aus_rest$restaurant == "Wendy's" |
  aus_rest$restaurant == "Little Caesars" |
  aus_rest$restaurant == "Taco Shack")


head(aus_rest_2)

aus_rest_2$inspection <- 1
aus_rest_2$low <- ifelse(aus_rest_2$score < 90, 1, 0)

risk <- aus_rest_2 %>%
  group_by(restaurant) %>%
  summarize(low_percent= sum(low)/sum(inspection))

risk$low_percent <- round(risk$low_percent * 100, 0)
risk$text <- paste(risk$low_percent, "%")
 
### Set image dimension
dev.new(width=10, height=8)

ggplot(aus_rest_2, aes(score)) +
  geom_histogram() + 
  facet_wrap(~ restaurant, ncol=6)  +
  geom_vline(xintercept = 90, colour="grey") +
  geom_text(data = risk, aes(label=text, 85, 50), size=3)

savePlot("austin_restaurants", "wmf", device = dev.cur())


