

# c. mcclintock
# geocoding addresses

# ................................................................

# load libraries
library(tidyverse)
library(RColorBrewer)
library(knitr)
library(scales)
library(gganimate)
library(lubridate)

# load data 
load("data/mpd-20.RData")

# ................................................................

summer <- subset(stops, stop_dt > "2020-05-25" & stop_dt < "2020-09-01")

?geocode_OSM()

summer$stop_location_block <- gsub("BLOCK OF ", "", summer$stop_location_block)

summer_addresses <- summer$stop_location_block

addresses1to5k <- geocode_OSM(summer_addresses[1:5000])

write.csv(addresses1to5k, "addresses1to5k.csv")
# 
# addresses1to5k <- geocode_OSM(summer_addresses[5001:10000])
# addresses1to5k <- geocode_OSM(summer_addresses[10001:13998])


