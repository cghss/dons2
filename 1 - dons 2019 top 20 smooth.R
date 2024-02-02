library(tidyverse)
library(mgcv)

dons <- read_csv("/Users/carlson/Documents/DON preliminary analysis/DONdatabase.csv")
#dons <- read_csv("/Users/carlson/Documents/DON preliminary analysis/DONdatabase2.csv")

dons %>%
  mutate(YearEvent = year(mdy(ReportDate))) %>%
  # mutate(YearEvent = interval(mdy(ReportDate), today()) %/% months(1)) %>%
  # select(YearEvent, DiseaseLevel1, Link) %>% distinct() %>%
  group_by(YearEvent, DiseaseLevel1) %>%
  summarize(Count = n()) %>%
  mutate(DiseaseLevel1 = as.factor(DiseaseLevel1)) %>%
  ungroup() -> don

don %>%
  group_by(DiseaseLevel1) %>%
  summarize(Count = sum(Count)) %>%
  arrange(-Count) %>%
#  slice_max(Count, n = 20) %>%
  pull(DiseaseLevel1) -> dis

#setdiff(dis, c("MERS-CoV", "Ebola virus", "SARS-CoV", "Zika virus disease", "Influenza A")) -> dis

don %>%
  filter(DiseaseLevel1 %in% dis) %>%
  group_by(DiseaseLevel1) %>%
  complete(YearEvent = 1996:2019,
           fill = list(Count = 0)) -> don
don %>% as.data.frame() -> don

don %>% rename(Year = YearEvent) -> don
g <- bam(Count ~ s(Year) + s(Year, DiseaseLevel1, bs = "fs"), family = "poisson", data = don)
library(gratia)
draw(g)

library(gratia)
p1 <- draw(g, select = "s(Year)") + ggtitle("(A) Overall trend")
p2 <- draw(g, select = "s(Year,DiseaseLevel1)") + ggtitle("(B) Disease-specific trends") + ylab("Year") +
  scale_color_manual(values=met.brewer("Cross", 67))
p1 + p2 + plot_layout(ncol = 2) & theme_bw() + theme(legend.position = 'n')

