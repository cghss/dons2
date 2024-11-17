# Load libraries
library(readr)
library(sf)
library(rnaturalearth)
library(dplyr)
library(ggplot2)
library(tiyverse)
library(viridisLite)

Donmap <- read.csv("DONSUpdate.csv") 

key <- c("JAP" = "JPN", "XKO" = "XKX")
Donmap %<>% mutate(ISO = recode(ISO, !!!key))

Donmap %>%
  count(ISO) -> df


# Merge world map data with dataset based on ISO codes
world_data <- merge(world, df, by.x = "iso_a3", by.y = "ISO", all.x = TRUE)


# Create a world map heat map 
world_heatmap <- ggplot() +
  geom_sf(data = world_data, aes(fill = n), color = "black", size = 0.2) +
  scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = "right",  
        legend.title = element_blank(),  
        legend.text = element_text(size = 10),  
        legend.key.size = unit(4, "lines"), 
        legend.key.height = unit(2.2, "cm"),  
        legend.key.width = unit(0.7, "cm"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
      ) +
  labs(fill = "Frequency")

print(world_heatmap)
