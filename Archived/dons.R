library(tidyverse)
library(mgcv)

dons <- read_csv("/Users/carlson/Documents/DON preliminary analysis/DONdatabase.csv")
#dons <- read_csv("/Users/carlson/Documents/DON preliminary analysis/DONdatabase2.csv")

dons %>%
  mutate(YearEvent = year(mdy(ReportDate))) %>%
  # mutate(YearEvent = interval(mdy(ReportDate), today()) %/% months(1)) %>%
  # group_by(YearEvent, DiseaseLevel1, Link) %>% distinct() %>%
  group_by(YearEvent, DiseaseLevel1) %>%
  summarize(Count = n()) %>%
  mutate(DiseaseLevel1 = as.factor(DiseaseLevel1)) %>%
  ungroup() -> don

don %>%
  group_by(DiseaseLevel1) %>%
  summarize(Count = sum(Count)) %>%
  arrange(-Count) %>%
  slice_max(Count, n = 20) %>%
  pull(DiseaseLevel1) -> dis

setdiff(dis, c("MERS-CoV", "Ebola virus", "SARS-CoV", "Zika virus disease", "Influenza A", "COVID-19")) -> dis

don %>%
  filter(DiseaseLevel1 %in% dis) %>%
  group_by(DiseaseLevel1) %>%
  complete(YearEvent = 1996:2019,
           fill = list(Count = 0)) -> don
don %>% as.data.frame() -> don

g <- bam(Count ~ s(YearEvent) + s(YearEvent, DiseaseLevel1, bs = "fs"), family = "poisson", data = don)
library(gratia)
draw(g)
