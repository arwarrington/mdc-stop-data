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
#rename districts
stopdata$stop_district[c("1D","2D","3D","4D","5D","6D","7D",NULL,"")]<-c(1,2,3,4,5,6,7,NULL,"")


map <- tidy(my_spdf, region = "NAME")


mapped_data <- inner_join(stopdata, map)

str(mapped_data)

ggplot(mapped_data) +
  geom_sf(aes(fill= ))+
  scale_fill_gradient(low="#56B1F7", high="#132B43")

