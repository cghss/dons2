library(tidyverse)
library(mgcv)
library(patchwork)
library(MetBrewer)

#dons <- read_csv("/Users/carlson/Documents/DON preliminary analysis/DONdatabase.csv")
dons <- read_csv("/Users/carlson/Documents/DON preliminary analysis/DONdatabase2.csv")

dons1 <- dons[1:3338,]
dons2 <- dons[3339:3625,]

dons1 %>%
  mutate(YearEvent = year(mdy(ReportDate))) %>%
  # mutate(YearEvent = interval(mdy(ReportDate), today()) %/% months(1)) %>%
  select(YearEvent, DiseaseLevel1, Link) %>% distinct() %>%
  group_by(YearEvent, DiseaseLevel1) %>%
  summarize(Count = n()) %>%
  mutate(DiseaseLevel1 = as.factor(DiseaseLevel1)) %>%
  ungroup() -> don

dons2 %>%
  mutate(YearEvent = year(dmy(ReportDate))) %>%
  # mutate(YearEvent = interval(mdy(ReportDate), today()) %/% months(1)) %>%
  select(YearEvent, DiseaseLevel1, Link) %>% distinct() %>%
  group_by(YearEvent, DiseaseLevel1) %>%
  summarize(Count = n()) %>%
  mutate(DiseaseLevel1 = as.factor(DiseaseLevel1)) %>%
  ungroup() -> don2

don %>% bind_rows(don2) -> don

#######

don %>%
  group_by(YearEvent) %>% 
  count() %>% ggplot(aes(x = YearEvent, y = n)) + geom_point()
