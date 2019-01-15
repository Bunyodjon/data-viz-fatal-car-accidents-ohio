# Data Viz Group Project 
# Authors: Ashlee, Tyler, Bunyod 
# Date: 11/15/2018
# Last Updated: 12/6/2018 

library(dplyr)
library(magrittr)
library(ggplot2)
library("ggmap")
library(devtools)
library(sas7bdat)
library(leaflet)
library(tidyverse)


# ------------------------ Loading Data  for 2013 - 2017 --------------------------------------#  
accident2017 = read.csv("accident2017.csv")
distract2017 = read.csv("Distract2017.csv")
person2017 = read.csv("person2017.csv")

accident2016 = read.csv("accident2016.csv")
distract2016 = read.csv("Distract2016.csv")
person2016 = read.csv("person2016.csv")

accident2015 = read.csv("accident2015.csv")
distract2015 = read.csv("Distract2015.csv")
person2015 = read.csv("person2015.csv")

accident2014 = read.sas7bdat("accident2014.sas7bdat")
distract2014 = read.sas7bdat("distract2014.sas7bdat")
person2014 = read.sas7bdat("person2014.sas7bdat")

accident2013 = read.sas7bdat("accident2013.sas7bdat")
distract2013 = read.sas7bdat("distract2013.sas7bdat")
person2013 = read.sas7bdat("person2013.sas7bdat")

# preparing distract data set 
distract2013 <- distract2013 %>% 
  filter(MDRDSTRD %in% c(5, 6, 13, 14, 92)) %>% 
  distinct(ST_CASE, .keep_all = TRUE)

distract2014 <- distract2014 %>% 
  filter(MDRDSTRD %in% c(5, 6, 13, 14, 92)) %>% 
  distinct(ST_CASE, .keep_all = TRUE)

distract2015 <- distract2015 %>% 
  filter(MDRDSTRD %in% c(5, 6, 13, 14, 92)) %>% 
  distinct(ST_CASE, .keep_all = TRUE)

distract2016 <- distract2016 %>% 
  filter(MDRDSTRD %in% c(5, 6, 13, 14, 92)) %>% 
  distinct(ST_CASE, .keep_all = TRUE)

distract2017 <- distract2017 %>% 
  filter(MDRDSTRD %in% c(5, 6, 13, 14, 92)) %>% 
  distinct(ST_CASE, .keep_all = TRUE)

# merge accident and distract data using ST_CASE ID, mutate cause of the accident. drop three variables    
accident2017 <- left_join(accident2017, distract2017, by="ST_CASE") %>% 
  mutate(MDRDSTRD2=ifelse(is.na(MDRDSTRD)==TRUE, 1, MDRDSTRD)) %>%
  select(-c("STATE.y", "VEH_NO", "MDRDSTRD"))

accident2016 <- left_join(accident2016, distract2016, by="ST_CASE") %>% 
  mutate(MDRDSTRD2=ifelse(is.na(MDRDSTRD)==TRUE, 1, MDRDSTRD)) %>%
  select(-c("STATE.y", "VEH_NO", "MDRDSTRD"))

accident2015 <- left_join(accident2015, distract2015, by="ST_CASE") %>% 
  mutate(MDRDSTRD2=ifelse(is.na(MDRDSTRD)==TRUE, 1, MDRDSTRD)) %>%
  select(-c("STATE.y", "VEH_NO", "MDRDSTRD"))

accident2014 <- left_join(accident2014, distract2014, by="ST_CASE") %>% 
  mutate(MDRDSTRD2=ifelse(is.na(MDRDSTRD)==TRUE, 1, MDRDSTRD)) %>%
  select(-c("STATE.y", "VEH_NO", "MDRDSTRD"))

accident2013 <- left_join(accident2013, distract2013, by="ST_CASE") %>% 
  mutate(MDRDSTRD2=ifelse(is.na(MDRDSTRD)==TRUE, 1, MDRDSTRD)) %>%
  select(-c("STATE.y", "VEH_NO", "MDRDSTRD"))


# Select only necessary variables
accident2017 <- accident2017 %>% 
  select(STATE.x, ST_CASE, PERSONS, CITY, DAY, MONTH, YEAR, DAY_WEEK, HOUR, MINUTE, 
         LATITUDE, LONGITUD, WEATHER, FATALS, MDRDSTRD2)

accident2016 <- accident2016 %>% 
  select(STATE.x, ST_CASE, PERSONS, CITY, DAY, MONTH, YEAR, DAY_WEEK, HOUR, MINUTE, 
         LATITUDE, LONGITUD, WEATHER, FATALS, MDRDSTRD2)

accident2015 <- accident2015 %>% 
  select(STATE.x, ST_CASE, PERSONS, CITY, DAY, MONTH, YEAR, DAY_WEEK, HOUR, MINUTE, 
         LATITUDE, LONGITUD, WEATHER, FATALS, MDRDSTRD2)

accident2014 <- accident2014 %>% 
  select(STATE.x, ST_CASE, PERSONS, CITY, DAY, MONTH, YEAR, DAY_WEEK, HOUR, MINUTE, 
         LATITUDE, LONGITUD, WEATHER, FATALS, MDRDSTRD2)

accident2013 <- accident2013 %>% 
  select(STATE.x, ST_CASE, PERSONS, CITY, DAY, MONTH, YEAR, DAY_WEEK, HOUR, MINUTE, 
         LATITUDE, LONGITUD, WEATHER, FATALS, MDRDSTRD2)

# rbind all the data set 
accident.all <- rbind(accident2013, accident2014, accident2015, accident2016, accident2017)

# ------------------------------------- WEATHER CODE ---------------------------------------------# 
# create other category for the weather variable
accident.all <- accident.all %>% 
  mutate(WEATHER=ifelse(WEATHER %in% c(1, 2, 3, 4, 5), WEATHER, 6))

# create label for the weather and call it WEATHER2 
accident.all$WEATHER2 <- factor(accident.all$WEATHER, labels = c("Clear", "Rain", "Sleet or Hail", "Snow", "Fog", "Other"))


# write to csv file for the app use
write.csv(accident.all, file="accident_all.csv", row.names = FALSE)


# -------------------------------------Map Code  ---------------------------------------------# 
# Filtering based on city: testing static version 
accident.cincinati <- accident.all %>% 
  filter((LONGITUD<=-84.2 & LONGITUD >=-84.6) & (LATITUDE >=39.02 & LATITUDE<=39.20))



# testing gmplot  - works but not a pretty plot 
map1 <- qmplot(LONGITUD, LATITUDE, data = accident.cincinati, maptype = "toner-lite", color = I("red"))
map1

# Create a palette that maps factor levels to colors
pal <- colorFactor(c("navy", "black", "cyan4", "deeppink", "purple", "red"), domain = c("Rain", "Snow", "Clear", "Fog", "Sleet or Hail", "Other"))

# Better looking map from leaflet package 
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng=accident.cincinati$LONGITUD, lat=accident.cincinati$LATITUDE, popup="Open Street Map",
                   radius=6, color=accident.cincinati$WEATHER,
                   stroke = FALSE, fillOpacity = 0.5, 
                   clusterOptions = markerClusterOptions())

m 

# --------------------------------------- Time Series Plot Code --------------------------------- # 
# This summarises data by Day Of A Week
accident_summary <- accident.all %>% 
  filter((LONGITUD<=-84.2 & LONGITUD >=-84.6) & (LATITUDE >=39.02 & LATITUDE<=39.20)) %>% 
  group_by(DAY_WEEK, WEATHER2) %>% 
  summarise(ACCIDENT_COUNT=n())


# This summarises  data by Hour of a Day
accident_summary1 <- accident.all %>% 
  filter((LONGITUD<=-84.2 & LONGITUD >=-84.6) & (LATITUDE >=39.02 & LATITUDE<=39.20)) %>% 
  group_by(HOUR, WEATHER2) %>% 
  summarise(ACCIDENT_COUNT=n()) 


# line plot with points 
ggplot() + 
  geom_point(aes(x=DAY_WEEK, y=ACCIDENT_COUNT, group=1), data=accident_summary) +
  geom_line(aes(x=DAY_WEEK, y=ACCIDENT_COUNT, group=1), data=accident_summary)


# Stacked bar plot 
g1 <- ggplot() + 
  geom_bar(aes(x=NUMBER, y=ACCIDENT_COUNT, fill=WEATHER2), 
           stat="identity", width=0.8, data=plotdata) +
  theme_classic()+
  scale_fill_discrete(label=c("Clear", "Rain", "Sleet or Hail", "Snow", "Fog", "Other"))+
  labs(y="Number of Fatal Accidents", fill="Weather",
       legend="Weather",
       title="Ohio State Fatal Accidents (2013 - 2017)",
       caption="National Highway Traffic Safety Administration (NHTSA) - Fatal Accident, 2013-2017") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(size=22, hjust = 0.5),
        axis.title.y = element_text(size=18, margin=margin(t = 0, r = 20, b = 0, l = 0)))


# Just testing several things and making sure we are recieving a correct data set and plot


# ----------------------------- Creating Proper Labels with Order -----------------------------#
# if 99 in HOUR column call it 99
accident.all$HOUR <- ifelse(accident.all$HOUR==99, "NA", accident.all$HOUR)

# create HOUR1 with labels 
accident.all$HOUR1 <- factor(accident.all$HOUR, labels=c("12:00am", "1:00am", "2:00am", "3:00am", 
                                                            "4:00am", "5:00am", "6:00am", "7:00am",
                                                            "8:00am", "9:00am", "10:00am", "11:00am",
                                                            "12:00pm", "1:00pm","2:00pm","3:00pm",
                                                            "4:00pm", "5:00pm", "6:00pm", "7:00pm",
                                                            "8:00pm", "9:00pm", "10:00pm", "11:00pm")) 

accident.all2 <- accident.all %>%
  mutate(HOUR1=factor(HOUR1, levels = c("12:00am", "1:00am", "2:00am", "3:00am", 
                               "4:00am", "5:00am", "6:00am", "7:00am",
                               "8:00am", "9:00am", "10:00am", "11:00am",
                               "12:00pm", "1:00pm","2:00pm","3:00pm",
                               "4:00pm", "5:00pm", "6:00pm", "7:00pm",
                               "8:00pm", "9:00pm", "10:00pm", "11:00pm")))


# create DAY_WEEK1 with labels 
accident.all$DAY_WEEK1 <- factor(accident.all$DAY_WEEK, labels = c("Sunday", "Monday", "Tuesday", 
                                                "Wednesday", "Thursday", "Friday", "Saturday"))
# create MONTH1 with labels 
accident.all$MONTH1 <- factor(accident.all$MONTH, labels =c("January", "Febraury", "March", "April", 
                                                            "May", "June", "July", "August",
                                                            "September", "October", "November",
                                                            "December")) 
# save the csv file 
#accident.all <- read_csv("Data/accident_all.csv")
write.csv(accident.all, "Data/accident_all.csv", row.names = F)


# testing if plotdata reactive part giving the correct data set  
plotdata <- accident.all %>%
  group_by_("HOUR1", "WEATHER2") %>%
  summarise(ACCIDENT_COUNT=n()) %>%
  ungroup() %>% 
  na.omit()
 
# testing if the static bar plot working fine
ggplot() + 
  geom_bar(aes(x=HOUR1, y=ACCIDENT_COUNT, fill=WEATHER2), 
           stat="identity", width=0.8, data=plotdata) +
  theme_classic()+
  scale_fill_discrete(label=c("Clear", "Rain", "Sleet or Hail", "Snow", "Fog", "Other"))+
  labs(y="Number of Fatal Accidents", fill="Weather",
       legend="Weather",
       title="Ohio Fatal Accidents (2013 - 2017)",
       caption="National Highway Traffic Safety Administration (NHTSA) - Fatal Accident, 2013-2017") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(size=22, hjust = 0.5),
        axis.title.y = element_text(size=18, margin=margin(t = 0, r = 20, b = 0, l = 0)),
        legend.position = "bottom") 

