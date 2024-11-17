library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

##Load data
co.raw <- read.csv("Data/covid_cases.csv")

##Mutate to pull out the cases by week
co.raw$Date <- mdy(co.raw$Date)
co.raw %>% 
  mutate(date = as.Date(Date)) %>% 
  group_by(week = isoweek(Date), year = isoyear(Date)) %>% 
  summarise(weekCases = sum(New)) -> codon

#Week calendar for duration of Covid data
data.frame(date=seq(as.Date("2020/1/1"), as.Date("2023/12/31"), by="day")) %>%
  mutate(week=isoweek(date),year=isoyear(date)) %>%
  group_by(year,week) %>%
  summarise(weekdate=min(date)) -> week_calendar2

codon %>%
  left_join(week_calendar2) -> codon

A <- ggplot(codon, aes(x = weekdate, y = weekCases)) +
  theme_bw() + 
  #geom_point(data = noco2,
  #aes(x = as.Date(Date),
  #y = weekcases, 
  #size = wordcount,
  # size = 1, 
  #color = "DON Report Word Count"),
  #color = '#ff854fff', 
  #alpha = 0.6,
  # shape = 16) +
  #labs(size = "Words per Report") +
  #scale_size_continuous(range = c(1, 15), limits = c(1,6500)) +
geom_vline(xintercept = as.numeric(as.Date("2020-01-30")), 
           color = "grey", 
           linetype = "dashed", 
           size = 0.8) +
  geom_line(color = "gray10") +
  labs(title = "A",
       x = "",
       y = "New COVID-19 Cases per Week") +
  theme(panel.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "right") +
  labs(fill = "Reporting frequency") +
  scale_y_continuous(limits = c(-0.01*max(codon$weekCases), 1*max(codon$weekCases)),
                     #breaks = seq(from = 0, to = 25000000, by = 5000000),
                     labels = scales::comma_format()); A
print(A)
