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
noco2 <- noco %>% filter(DON == "COVID")
noco2$weekcases <- c(0, 0, 2120, 2120, 4083599, 5219504, 4012227, 4424486)
noco2$wordcount <- c(1157, 1133, 1301, 1118, 1710, 1788, 1945, 2917)
A <- ggplot(codon, aes(x = weekdate, y = weekCases)) +
  theme_bw() + 
  geom_point(data = noco2,
             aes(x = as.Date(Date),
                 y = weekcases, 
                 size = wordcount, 
                 color = "DON Report Word Count"),
             color = '#ff854fff', 
             alpha = 0.6,
             shape = 16) +
  labs(size = "Words per Report") +
  scale_size_continuous(range = c(1, 15), limits = c(1,6500)) +
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
  labs(fill = "Reporting frequency") +
  scale_y_continuous(limits = c(-0.01*max(codon$weekCases), 1*max(codon$weekCases)),
                     #breaks = seq(from = 0, to = 25000000, by = 5000000),
                     labels = scales::comma_format()); A

nopox2 <- nopox %>% filter(DON == "MPOX")
nopox2$weekcases <- c(135, 135, 187, 615, 530, 1050, 1350, 1800, 200) #Number of cases per week corresponding to the date of the MPOX dons publication
nopox2$wordcount <- c(1783, 1758, 4909, 6403, 5301, 6266, 5565, 3726, 5333) #MPOX dons word count per report
#Mpox Figure
B <- ggplot(dompox, aes(x = weekdate, y = weekCases)) +
  theme_bw() + 
  geom_point(data = nopox2,
                      aes(x = as.Date(date),
                            y = weekcases,
                            size = wordcount, 
                          color = "DON Report Word Count"),  
             alpha = 0.6,
             color = '#ff854fff', 
             shape = 16) +
  scale_size_continuous(range = c(1, 15), limits = c(1,6500)) +
  labs(size = "Words per Report") +
  geom_vline(xintercept = as.numeric(as.Date("2022-07-23")), 
             color = "grey", 
             linetype = "dashed", 
             size = 0.8) +
  geom_line(color = "gray10") +
  labs(title = "B",
       x = "",
       y = "New Cases per Week") +
  theme(panel.background = element_blank()) + 
  scale_y_continuous(limits = c(-0.01*max(dompox$weekCases), 1*max(dompox$weekCases)),
                     #breaks = seq(from = -2000, to = 8000 , by = 1000),
                     labels = scales::comma_format()); B

#Join em together
(A + theme(legend.position = "none")) / B + plot_layout(guides = "collect") #& 
  #scale_size_continuous(limits = range(c(noco2$wordcount, nopox2$wordcount)))
 
