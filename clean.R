
# c. mcclintock
# defund mpd - now

# ................................................................

# load libraries
library(tidyverse)
library(RColorBrewer)
library(knitr)
library(scales)
library(gganimate)
library(lubridate)

# read in the data 
stops1 <- read_csv("data/stops-janmar20.csv", na = "NULL", col_types = cols(.default = "c"))
stops2 <- read_csv("data/stops-marjun20.csv", na = "NULL", col_types = cols(.default = "c"))
stops3 <- read_csv("data/stops-julsept20.csv", na = "NULL", col_types = cols(.default = "c"))
stops4 <- read_csv("data/stops-octdec20.csv", na = "NULL", col_types = cols(.default = "c"))
# ................................................................

# join time periods together
stops <- full_join(stops1, stops2, by=names(stops1))
stops <- full_join(stops, stops3, by=names(stops1))
stops <- full_join(stops, stops4, by=names(stops1))

# categorical duration values
stops$stop_duration_minutes <- as.numeric(stops$stop_duration_minutes)
stops$duration_cut <- cut(stops$stop_duration_minutes, 
                          breaks = c(1,5,20,60, 120, 360), 
                          include.lowest = T)

# create subset for youth
youth <- subset(stops, age=="Juvenile")

# age to numeric
stops$age <- as.numeric(stops$age)

# make missing race explicit
stops$race_ethnicity <- fct_explicit_na(stops$race_ethnicity)

# ................................................................


# recode person_search_reason_probable_cause to separate variables
stops$psrpc_nature <- ifelse(str_detect(stops$person_search_reason_probable_cause, "Nature")==T, "nature of the alleged crime", NA)
stops$psrpc_char <- ifelse(str_detect(stops$person_search_reason_probable_cause, "Characteristics")==T, "characteristics of an armed individual", NA)
stops$psrpc_witness <- ifelse(str_detect(stops$person_search_reason_probable_cause, "witnesses")==T, "information from witnesses", NA)
stops$psrpc_actions <- ifelse(str_detect(stops$person_search_reason_probable_cause, "actions")==T, "individual actions", NA)
stops$psrpc_consent <- ifelse(str_detect(stops$person_search_reason_probable_cause, "Consent")==T, "consent", NA)
stops$psrpc_infoleo <- ifelse(str_detect(stops$person_search_reason_probable_cause, "law enforcement")==T, "information from law enforcement", NA)

# race demographics 
prop.table(table(stops$psrpc_nature, stops$race_ethnicity)) # 90% of 524
prop.table(table(stops$psrpc_char, stops$race_ethnicity)) # 96.4% of 112
prop.table(table(stops$psrpc_witness, stops$race_ethnicity)) # 94.9% of 99
prop.table(table(stops$psrpc_actions, stops$race_ethnicity)) # 91.3% of 496
prop.table(table(stops$psrpc_consent, stops$race_ethnicity)) # 87% of 31
prop.table(table(stops$psrpc_infoleo, stops$race_ethnicity)) #93.9%  of 412

# race by stop district
ggplot(subset(stops, !is.na(stop_district)), aes(stop_district, fill=race_ethnicity)) + 
  geom_bar(position="fill") + 
  scale_fill_manual(values=brewer.pal(8, "Set2")) +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Race of People Stopped by Police District", 
       fill="Race/Ethnicity", 
       x="District", y="Percent") 

# stop duration by race
ggplot(stops, aes(duration_cut, fill=race_ethnicity)) + geom_bar(position="fill")

# and median and mean
stops %>% 
  group_by(race_ethnicity) %>% 
  summarize(median=median(stop_duration_minutes, na.rm = T), 
            mean=mean(stop_duration_minutes, na.rm=T))

# youth stop race demographics
table(youth$race_ethnicity) %>% prop.table()

# overall stop race demographics
table(stops$race_ethnicity) %>% prop.table()


# age by race
ggplot(stops, aes(age, fill=race_ethnicity)) + geom_density(alpha=0.6)

# stop type by race
kable(100*prop.table(table(stops$stop_type, stops$race_ethnicity), margin=2), digits = 2)
# black citizens who were stopped were more likely to have a non-ticket stop 
# evidence of racial profiling

prop.table(table(stops$property_protective_pat_down, stops$race_ethnicity), margin=2)
prop.table(table(stops$person_search_consent, stops$race_ethnicity), margin=2)
prop.table(table(stops$person_search_probable_cause, stops$race_ethnicity), margin=2)
prop.table(table(stops$person_search_or_protective_pat_down, stops$race_ethnicity), margin=2)
# 17% Black, 3.3% white


# ................................................................

rm(list=ls()[! ls() %in% c("stops","youth")])
save.image("data/mpd-20.RData")
