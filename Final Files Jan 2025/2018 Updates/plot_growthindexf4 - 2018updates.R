library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

#contains code to plot both growth indices and nest counts
#updated April 2025 to improve legibility of plots

setwd("C:/Users/baill/OneDrive/Desktop/thesis_dcco_bcnh_2025/Final Files Jan 2025/2018 Updates")

combined_datasetMar2025 <- read_csv("combined_datasetMarch2025.csv")

#import dcco and bcnh nestcounts 
dcco_nestcount <- read_csv("C:/Users/baill/OneDrive/Desktop/thesis_dcco_bcnh_2025/Final Files Jan 2025/2018 Updates/dcco_data.csv")
bcnh_nestcount <- read_csv("C:/Users/baill/OneDrive/Desktop/thesis_dcco_bcnh_2025/Final Files Jan 2025/2018 Updates/bcnh_data.csv")

dcco_data <- combined_datasetMar2025 %>% 
  select(year, dcco_growthindex) %>%
  rename(growth_index = dcco_growthindex)

bcnh_data <- combined_datasetMar2025 %>% 
  select(year, bcnh_growthindex) %>%
  rename(growth_index = bcnh_growthindex)

# plot both growth indexes together with vertical line at 2009
ggplot() +
  # Cormorant line with circles
  geom_line(data = transform(dcco_data, year = as.numeric(as.character(year))), 
            aes(x = year, y = growth_index, group=1, color = "cormorants"), linetype = "solid") +
  geom_point(data = transform(dcco_data, year = as.numeric(as.character(year))),
             aes(x = year, y = growth_index, color = "cormorants"), shape = 16, size = 3) +
  # Night-heron line with triangles
  geom_line(data = transform(bcnh_data, year = as.numeric(as.character(year))), 
            aes(x = year, y = growth_index, group=1, color = "night-herons"), linetype = "dotted") +
  geom_point(data = transform(bcnh_data, year = as.numeric(as.character(year))),
             aes(x = year, y = growth_index, color = "night-herons"), shape = 17, size = 3) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2009, y = 0.5, 
           label = "Management starts", angle = 90, size = 4, vjust = -0.5) +
  scale_x_continuous(limits = c(1992, 2023), breaks = seq(1995, 2020, by = 5)) +
  labs(title = "",
       x = "Year", y = "Growth Index", color = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, vjust = 0.5),
        plot.margin = margin(t = 10, r = 10, b = 20, l = 30, unit = "pt"),
        legend.text = element_text(size = 12)
        )

#save as png to current working directory
ggsave("growthindexplotApril2025.png", width = 8, height = 6, dpi = 600)

#plot both nest count datasets over years in the same theme as the growth index plot
ggplot() +
  # Cormorant line with circles
  geom_line(data = dcco_nestcount, aes(x = year, y = dcco_count, group=1, color = "cormorants"), linetype = "solid") +
  geom_point(data = dcco_nestcount, aes(x = year, y = dcco_count, color = "cormorants"), shape = 16, size = 3) +
  # Night-heron line with triangles  
  geom_line(data = bcnh_nestcount, aes(x = year, y = bcnh_count, group=1, color = "night-herons"), linetype = "dotted") +
  geom_point(data = bcnh_nestcount, aes(x = year, y = bcnh_count, color = "night-herons"), shape = 17, size = 3) +
 # geom_vline(xintercept = 2009, linetype = "dashed", color = "gray50") +
 # annotate("text", x = 2009, y = 3000, 
#         label = "Management starts", angle = 90, size = 4, vjust = -0.5) +
  scale_x_continuous(limits = c(1992, 2023), breaks = seq(1995, 2020, by = 5)) +
  labs(title = "",
       x = "Year", y = "Nest Count", color = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, vjust = 0.5),
        plot.margin = margin(t = 10, r = 10, b = 20, l = 30, unit = "pt"),
        legend.text = element_text(size = 12)
        )

#save as high resolution png to current wd
ggsave("nestcountplotApril2025.png", width = 8, height = 6, dpi = 600)