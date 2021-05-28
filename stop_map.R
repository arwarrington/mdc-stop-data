library(tidyverse)
library(rgdal)
library(broom)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

stopdata <- read.table("~/Desktop/R/mdc_stops/stopdata.csv", header=TRUE,fill=TRUE, sep=',')
system("unzip ~/Desktop/R/mdc_stops/Police_Districts.zip")
my_spdf <- readOGR(dsn="~/Desktop/R/mdc_stops/Police_Districts/",
                   layer="Police_Districts",
                    verbose=FALSE)
map <- tidy(my_spdf, region = "NAME")

mapped_data <- inner_join(stopdata, map)

str(mapped_data)

ggplot(mapped_data) +
  geom_sf(aes(fill= MedianIncome))+
  scale_fill_gradient(low="#56B1F7", high="#132B43")

