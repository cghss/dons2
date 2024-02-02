library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(patchwork)

##Load data
co.raw <- read.csv("mergeddonscovid.csv")
dompox.raw <- read.csv("dompox_curve.csv")

##Mutate to pull out the cases by week
co.raw$Date <- mdy(co.raw$Date)
co.raw %>% 
  mutate(date = as.Date(Date)) %>% 
  group_by(week = isoweek(Date), year = isoyear(Date)) %>% 
  summarise(weekCases = sum(New)) -> codon

dompox.raw$date <- mdy(dompox.raw$date)
dompox.raw %>%
  mutate(date = as.Date(date)) %>%
  group_by(week =isoweek(date), year = isoyear(date)) %>%
  summarise(weekCases = sum(NewCases)) -> dompox

#Week calendar for duration of Mpox data
data.frame(date=seq(as.Date("2022/1/1"), as.Date("2023/12/31"), by="day")) %>% 
  mutate(week=isoweek(date),year=isoyear(date)) %>%
  group_by(year,week) %>% 
  summarise(weekdate=min(date)) -> week_calendar

#Week calendar for duration of Covid data
data.frame(date=seq(as.Date("2020/1/1"), as.Date("2023/12/31"), by="day")) %>%
  mutate(week=isoweek(date),year=isoyear(date)) %>%
  group_by(year,week) %>%
  summarise(weekdate=min(date)) -> week_calendar2

#Join week calendars with respective dataframes
codon %>%
  left_join(week_calendar2) -> codon
dompox %>%
  left_join(week_calendar) -> dompox

#Pull out the dates without applicable dons
noco <- co.raw[which(co.raw$DON!="NO"),] %>%
  mutate(Date = as.Date(Date))
nopox <- dompox.raw[which(dompox.raw$DON!="NO"),] %>%
  mutate(date. = as.Date(date))

#Pull out dates without SitReps for Covid
sit_rep_df <- co.raw[which(co.raw$SitRep!="NO"),] %>%
  mutate(Date = as.Date(Date))

##Covid figure
A <- ggplot(codon, aes(x = weekdate, y = weekCases)) +
  geom_point(data = noco,
             aes(x = as.Date(noco$Date),
                 y = 1.25 * max(codon$weekCases),
                 color = noco$DON),
             alpha = 0.9,
             size = 0.25) +
  geom_point(data = sit_rep_df,
             shape = 24, stroke = NA,
             aes(x = as.Date(sit_rep_df$Date),
                 y = 1.2 * max(codon$weekCases),
                 fill = sit_rep_df$SitRep),
             alpha = 0.9,
             size = 1) +
  scale_color_manual(values = c("#16256B", "#F9CE99")) +
  scale_fill_manual(values = c("#fc9272", "#2ca25f")) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-11")), 
             color = "grey", 
             linetype = "dashed", 
             size = 0.8) +
  geom_line(color = "gray10") +
  labs(title = "A",
       x = "",
       y = "New Cases per Week") +
  theme(panel.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "right") +
  labs(fill = "Reporting frequency",
       color = "DONS Topics") +
  scale_y_continuous(limits = c(0, 1.3*max(codon$weekCases)),
                     breaks = seq(from = 0, to = 25000000, by = 5000000),
                     labels = scales::comma_format())

#Mpox Figure
B <- ggplot(dompox, aes(x = weekdate, y = weekCases)) +
  geom_point(data = nopox,
             aes(x = as.Date(nopox$date),
                 y = 1.15*max(dompox$weekCases),
                 color = nopox$DON),
             alpha = 0.9, 
             size = 0.25) +
  geom_vline(xintercept = as.numeric(as.Date("2022-07-23")), 
             color = "grey", 
             linetype = "dashed", 
             size = 0.8) +
  scale_color_manual(values = c("#16256B", "#F9CE99")) +
  geom_line(color = "gray10") +
  labs(title = "B",
       x = "",
       y = "New Cases per Week") +
  theme(panel.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "right") +
  labs(color = "DONS Topics") +
  scale_y_continuous(limits = c(0, 1.3*max(dompox$weekCases)),
                     breaks = seq(from = 0, to = 8000 , by = 1000),
                     labels = scales::comma_format())

#Join em together
A + B
