rm(list=ls())

library(sf)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(mapview)
library(lubridate)

#Crime data
Crime_2020 <- read.csv("C:\\Users\\shuji\\Documents\\Upenn\\Planning by Number\\Final\\Data\\Crime\\Crime Incidents from 2020.csv")

colnamCleaning<-c("the_geom", "cartodb_id", "the_geom_webmercator")
Crime_2020 <-Crime_2020[ , -which(names(Crime_2020) %in% colnamCleaning)]


Crime_2019 <- read.csv("C:\\Users\\shuji\\Documents\\Upenn\\Planning by Number\\Final\\Data\\Crime\\Crime Incidents from 2019.csv")
Crime_2018 <- read.csv("C:\\Users\\shuji\\Documents\\Upenn\\Planning by Number\\Final\\Data\\Crime\\Crime Incidents from 2018.csv")
Crime_2017 <- read.csv("C:\\Users\\shuji\\Documents\\Upenn\\Planning by Number\\Final\\Data\\Crime\\Crime Incidents from 2017.csv")
Crime_2016 <- read.csv("C:\\Users\\shuji\\Documents\\Upenn\\Planning by Number\\Final\\Data\\Crime\\Crime Incidents from 2016.csv")


Crime <-
  rbind(Crime_2020,Crime_2019,Crime_2018,Crime_2017,Crime_2016)

Crime %>%
  mutate(hour_ = factor(hour_, levels = c(as.character(seq(0,24)))))%>%
  group_by(hour_)%>%
  tally()%>%
  group_by(hour_)%>%
  summarise(SUM_HOUR = sum(n)) %>%
  filter(!hour_ == "99")%>%
  ggplot(aes(hour_, SUM_HOUR, fill=hour_)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") 

Crime$hour_ <- as.numeric(Crime$hour_)


Crime %>%
  mutate(TIME_OF_DAY = case_when(hour_ < 7 | hour_ > 18 ~ "Overnight",
                                 hour_ >= 7 & hour_ < 10 ~ "AM Rush",
                                 hour_ >= 10 & hour_ < 15 ~ "Mid-Day",
                                 hour_ >= 15 & hour_ <= 18 ~ "PM Rush"))%>%
  group_by(hour_, TIME_OF_DAY) %>%
  tally() %>%
  group_by(TIME_OF_DAY) %>%
  summarise(sum_crime = sum(n))%>%
  ggplot(aes(TIME_OF_DAY, sum_crime, fill = TIME_OF_DAY))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean")
