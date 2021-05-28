

# c. mcclintock
# animated gifs of time data

# ................................................................


# load libraries
library(tidyverse)
library(RColorBrewer)
library(knitr)
library(scales)
library(gganimate)

# load data 
load("data/mpd-20.RData")

# ................................................................

# data variable to proper format
stops$stop_date <- mdy(stops$stop_date)

# count stops per day
daily <- stops %>% group_by(stop_date) %>% count()

# rolling average of stops every seven days
daily$sevenday <- zoo::rollmean(daily$n, k = 7, fill = NA)

# stops every day for 2020
ggplot(daily, aes(stop_date)) + 
  geom_point(aes(y=n)) + geom_line(aes(y=n)) + 
  geom_point(aes(y=sevenday), color="red") + geom_line(aes(y=sevenday), color="red") + 
  theme_minimal() + 
  transition_reveal(stop_date)

# create day of week variable
daily$dow <- wday(daily$stop_date, label=T)

# stops every day of the week for 2020
ggplot(daily, aes(dow, n, fill=dow)) + geom_bar(stat="identity") + 
  guides(fill=F) +
  transition_reveal(week(stop_date))




