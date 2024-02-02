library(tidyverse)
library(readr)
library(lubridate)
library(dplyr)
library(rstatix)
library(ggplot2)
library(patchwork)

#load data
donold <- read_csv("DONSOriginal.csv")

donnew <- read_csv("DONSUpdate.csv")

#Get rid of extra columns in Old Don Dataset
donold <- donold[, c("DiseaseLevel1", "ReportDate", "Headline", "Country", "ISO")]

#Set up a YearEvent column for both dataframes
donold$ReportDate <- mdy(donold$ReportDate)
donold$YearEvent <- year(donold$ReportDate)

donnew %<>%
  mutate(FormattedDate = format(dmy(ReportDate), "%m-%d-%Y"),
         YearEvent = year(mdy(FormattedDate)))

#Deduplicate by ensuring unique values in both the date and the title

deduplicated_donold <- donold %>%
  distinct(Headline, ReportDate, .keep_all = TRUE)

deduplicated_donnew <- donnew %>%
  distinct(Headline, ReportDate, .keep_all = TRUE)

#create a new dataframe of the counts for each year

ByYearOldDon <- deduplicated_donold %>%
  count(YearEvent, name = "Count") %>%
  rename(Year = YearEvent)

ByYearNewDon <- deduplicated_donnew %>%
  count(YearEvent, name = "Count") %>%
  rename(Year = YearEvent)

combined_data <- bind_rows(
  mutate(ByYearOldDon, Group = "Old"),
  mutate(ByYearNewDon, Group = "New")
)

# Doing the t-test
t_test_result <- t.test(Count ~ Group, data = combined_data)

# Boxplot
oldnewbox <- ggplot(combined_data, aes(x = Group, y = Count, color = Group)) +
  geom_boxplot() +
  labs(title = "Gross DONs Publications 1996-2019 versus 2020-2023",
       x = "Group", y = "Count") +
  stat_compare_means(comparisons = list(c("Old", "New")), method = "t.test", test = t_test_result,
                     label = "p.format", symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                                            symbols = c("***", "**", "*", "n.s."))) +
  scale_color_manual(values = c("Old" = lacroix_palettes$PassionFruit[7], "New" = lacroix_palettes$PassionFruit[1])) + 
  theme_classic()




###Now I want to know if there is a statistically significant difference in the 
#number of published dons in years 1996-2019 and then 2020-2023 that there were PHEICs announced and years that there were not 

#The concept of PHEIC is established in 2005 with the revision of the IHR. We have filtered out years before 2006
#to bound this analysis by the concept of PHEIC

#2006-2019
ByYearOldDon <- ByYearOldDon %>%
  filter(Year >= 2006)

ByYearOldDon <- ByYearOldDon %>%
  mutate(Event = ifelse(Year %in% c(2009, 2010, 2014, 2015, 2016, 2019), "PHEIC", "NONE"))

PHEICt19 <- t.test(Count ~ Event, data = ByYearOldDon)

print(PHEICt19)

outbreakbox <- ggplot(ByYearOldDon, aes(x = Event, y = Count, color = Event)) +
  geom_boxplot() +
  labs(title = "Annual DONs Publications during PHEIC Declaration Years 2006-2019",
       x = "Event", y = "Count") +
  stat_compare_means(comparisons = list(c("PHEIC", "NONE")), method = "t.test", test = PHEICt23,
                     label = "p.format", symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                                            symbols = c("***", "**", "*", "n.s."))) +
  scale_color_manual(values = c("PHEIC" = lacroix_palettes$PassionFruit[7], "NONE" = lacroix_palettes$PassionFruit[1])) + 
  theme_classic()



#2020-2023
ByYearNewDon <- ByYearNewDon %>%
  mutate(Event = ifelse(Year %in% c(2020, 2022), "PHEIC", "NONE"))

PHEICt23 <- t.test(Count ~ Event, data = ByYearNewDon)

print(PHEICt23)

pheicnewbox <- ggplot(ByYearNewDon, aes(x = Event, y = Count, color = Event)) +
  geom_boxplot() +
  labs(title = "Annual DONs Publications during PHEIC Declaration Years 2020-2023",
       x = "Event", y = "Count") +
  stat_compare_means(comparisons = list(c("PHEIC", "NONE")), method = "t.test", test = PHEICt23,
                     label = "p.format", symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                                            symbols = c("***", "**", "*", "n.s."))) +
  scale_color_manual(values = c("PHEIC" = lacroix_palettes$PassionFruit[7], "NONE" = lacroix_palettes$PassionFruit[1])) + 
  theme_classic()


###Now I am interested in the number of annual reports in PHEIC declaration years before and after COVID
cool_data <- bind_rows(
  mutate(ByYearOldDon, Dataset = "Old"),
  mutate(ByYearNewDon, Dataset = "New")
)

cool_data <- cool_data %>%
  mutate(Outbreak = ifelse(Year %in% c(2009, 2014, 2016, 2019), "Old PHEIC",
                           ifelse(Year %in% c(2020, 2022), "New PHEIC", "NO")))

OldvNewt <- t.test(Count ~ Outbreak, data = cool_data, subset = Outbreak %in% c("Old PHEIC", "New PHEIC"))

print(OldvNewt)

pheicsbox <- ggplot(filter(cool_data, Outbreak %in% c("Old PHEIC", "New PHEIC")), 
       aes(x = Outbreak, y = Count, color = Outbreak)) +
  geom_boxplot() +
  labs(title = "Annual DONs Publications in PHEIC Declaration Years 1996-2019 versus 2020-2023",
       x = "Outbreak", y = "Count") +
  stat_compare_means(comparisons = list(c("Old PHEIC", "New PHEIC")), method = "t.test", test = OldvNewt,
                     label = "p.format", symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                                            symbols = c("***", "**", "*", "n.s."))) +
  scale_color_manual(values = c("Old PHEIC" = lacroix_palettes$PassionFruit[7], "New PHEIC" = lacroix_palettes$PassionFruit[1])) + 
  theme_classic()

(oldnewbox | outbreakbox) /
  (pheicnewbox |pheicsbox)

