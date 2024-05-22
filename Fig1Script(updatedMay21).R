#load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(LaCroixColoR)
library(patchwork)
library(magrittr)
library(viridisLite)

###DONS Map###

#Load Map data
Donmap <- read_csv("DONSUpdate.csv") 

world1 <- ne_countries()

key <- c("JAP" = "JPN", "XKO" = "XKX")
Donmap %<>% mutate(ISO = recode(ISO, !!!key))

Donmap %>%
  count(ISO) -> df


# Merge world map data with dataset based on ISO codes
world_data <- merge(world1, df, by.x = "iso_a3_eh", by.y = "ISO", all.x = TRUE)


# Create a world map 
world_heatmap <- ggplot() +
  geom_sf(data = world_data, aes(fill = n), color = "black", size = 0.2) +
  scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  theme(
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    #axis.text = element_blank(),
    #axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.size = unit(4, "lines"),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.7, "cm"), 
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  labs(fill = "Frequency") + 
  coord_sf(expand = FALSE) + ggtitle("WHO Disease Outbreak News Reports (2020-2023)"); world_heatmap



###Bar Charts###

#load data
donu_raw <- read_csv("DONSUpdate.csv")

#mutate dates for consistency
donu_raw %<>%
  mutate(FormattedDate = format(dmy(ReportDate), "%m-%d-%Y"),
         YearEvent = year(mdy(FormattedDate)))

donu <- donu_raw %>%
  filter(!duplicated(donu_raw[c("Headline", "ReportDate")]))

##Make the Top 5 Disease Bar Chart
counted_donu <- donu %>%
  count(DiseaseLevel1, name = "Count") %>%
  arrange(desc(Count)) %>%
  top_n(5)

#printing top five diseases, if interested in raw numbers
#print(counted_donu)

disease_bar<- ggplot(counted_donu, aes(x = reorder(DiseaseLevel1, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = lacroix_palettes$PassionFruit[7]) +
  labs(x = "", y = "Number of Reports") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 50)) +
  ggtitle("Diseases")

##Make Top 5 Country Bar Chart, excluding 'Global'
# Filter out entries with "Global" in the Country column
filtered_donu <- donu %>%
  filter(Country != "Global")

cocount_donu <- filtered_donu %>%
  count(Country, name = "Count") %>%
  arrange(desc(Count)) %>%
  top_n(5)

#printing top five minus global, if interested in raw numbers
#print(cocount_donu)

country_bar <- ggplot(cocount_donu, aes(x = reorder(Country, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = lacroix_palettes$PassionFruit[7]) +
  #geom_text(aes(label = Count, vjust = -0.1, hjust = -0.9, color = "black", size = 3) + 
  labs(x = "", y = "Number of Reports") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(labels=c("DRC", "UK", "Saudi Arabia", "Nigeria", "China", "Guinea", "Uganda"))  +
  ggtitle("Countries")



#Make combination diseaselevel1 and country subfigure

# Filter out entries with "Global" in the Country column
filtered_donu <- donu %>%
  filter(Country != "Global")

combocount_donu <- filtered_donu %>%
  count(Country, DiseaseLevel1, name = "Count") %>%
  arrange(desc(Count)) %>%
  top_n(5, wt = Count)

#print raw numbers, if interested
print(combocount_donu)


custom_xlab <- paste(combocount_donu$Country, combocount_donu$DiseaseLevel1, sep = '/n', "and", '/n')

country_disease_bar <- ggplot(combocount_donu, aes(x = reorder(paste(Country, DiseaseLevel1), -Count, levels = custom_xlab), y = Count)) +
  geom_bar(stat = "identity", fill = lacroix_palettes$PassionFruit[7]) +
  labs(x = "", y = "Number of Reports") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(labels=c("United Kingdom Monkeypox" = "UK - Monkeypox",
                            "United Arab Emirates MERS-CoV" = "UAE - MERS-CoV",
                            "Equatorial Guinea Marburg fever" = "Guinea - Marburg",
                            "Uganda Ebola virus" = "Uganda - Ebola",
                            "Saudi Arabia MERS-CoV" = "Saudi Arabia - MERS-CoV", 
                            "Democratic Republic of the Congo Ebola virus" = "DRC - Ebola"))  +
  ggtitle("Outbreaks")


##arrange the four plots

world_heatmap / plot_spacer() / {
  disease_bar + country_bar + country_disease_bar + plot_layout(axis_titles = "collect")
  } +
  plot_layout(nrow = 3, heights = c(2, 0.1, 1.2)) #+ 
  #plot_annotation(tag_levels = 'A')
 
  

