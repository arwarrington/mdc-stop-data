

# c. mcclintock
# animated gifs of time data

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

# create date/time variable
stops <- unite(stops, "stop_dt", stop_date, stop_time, sep = " ", remove=F)
stops$stop_dt <- mdy_hm(stops$stop_dt)

# data variable to proper format
stops$stop_date <- mdy(stops$stop_date)
stops$month <- month(stops$stop_dt, label = TRUE)
stops$year <- year(stops$stop_dt)
stops$wday <- wday(stops$stop_dt, label = TRUE)
stops$hour <- hour(stops$stop_dt)

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

# .............................................................................

# code borrowed with gratitude to little miss data: https://www.littlemissdata.com/blog/heatmaps

# create day/hour chart
dayhour <- stops %>% dplyr::group_by(wday, hour) %>% dplyr::count()

# reverse levels for figure display
dayhour$wday <- factor(dayhour$wday, levels=rev(levels(dayhour$wday)))

# add colors
col1 = "#d8e1cf" 
col2 = "#438484"

# make visualization!
ggplot(subset(dayhour, !is.na(wday)), aes(hour, wday)) + geom_tile(aes(fill = n),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Incidents")) +
  theme_bw() + theme_minimal() + 
  labs(title = "MPD Stops by Day of Week and Hour",
       x = "Stops Per Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# let's add a a cyclical component
# create week variable
stops$week <- week(stops$stop_date)

# create data set again
dayhour <- stops %>% dplyr::group_by(week, wday, hour) %>% dplyr::count()
# dayhour <- stops %>% dplyr::group_by(month, wday, hour) %>% dplyr::summarize(n=sum(n, na.rm = T))
dayhour$wday <- factor(dayhour$wday, levels=rev(levels(dayhour$wday)))

# render heat map that changes every week
ggplot(subset(dayhour, !is.na(wday)), aes(hour, wday)) + geom_tile(aes(fill = n),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Incidents")) +
  theme_bw() + theme_minimal() + 
  labs(title = "MPD Stops by Day of Week and Hour",
       x = "Stops Per Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_time(week)

test <- stops$stop_location_block[1:10]


