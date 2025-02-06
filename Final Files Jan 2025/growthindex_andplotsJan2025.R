library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

#file: Jan 2025 data check and re-calcalculation
#loads data for both species
#plots against management activities commencing
#re-calculates colony growth index

setwd("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo")

# Load the data
dcco_bcnh_data <- read_csv("dcco_bcnh_database_ 1992_2023_july30th.csv")
cols(dcco_bcnh_data)

#ensure all columns are numeric
dcco_bcnh_data <- dcco_bcnh_data %>% mutate_all(as.numeric)
#if cell is empty treat as zero
dcco_bcnh_data[is.na(dcco_bcnh_data)] <- 0

sum (dcco_bcnh_data$BCNH2016)

# Filter the data - pull columns starting with bcnh
bcnh_data <- dcco_bcnh_data %>% select(starts_with("BCNH")) %>%
#ensure all columns are numeric
  mutate_all(as.numeric) %>%
#clean column titles- remove BCNH from start of column name and keep numeric only 
  rename_all(~str_remove(., "BCNH")) %>%
#convert to long format for plotting
  pivot_longer(cols = everything(), names_to = "year", values_to = "bcnh_count") %>%
#group by year and sum the counts
  group_by(year) %>%
  summarise(bcnh_count = sum(bcnh_count))

bcnh_data <- bcnh_data %>%
  mutate(year = as.numeric(year)) %>%
  arrange(year)
print(bcnh_data)

# Repeat all above for DCCO data
dcco_data <- dcco_bcnh_data %>% select(starts_with("DCCO")) %>%
  mutate_all(as.numeric) %>%
  rename_all(~str_remove(., "DCCO")) %>%
  pivot_longer(cols = everything(), names_to = "year", values_to = "dcco_count") %>%
  group_by(year) %>%
  summarise(dcco_count = sum(dcco_count))

# Plot dcco over time  ----
ggplot(dcco_data, aes(x = year, y = dcco_count, group=1)) +
  geom_line() +
  labs(title = "DCCO over time",
       x = "Year",
       y = "Count")

#Plot bcnh over time 
ggplot(bcnh_data, aes(x = year, y = bcnh_count, group=1)) +
  geom_line() +
  labs(title = "BCNH over time",
       x = "Year",
       y = "Count")

#plot both together 
ggplot() +
  geom_line(data = dcco_data, aes(x = year, y = dcco_count, group=1, color = "DCCO")) +
  geom_line(data = bcnh_data, aes(x = year, y = bcnh_count, group=1, color = "BCNH")) +
  labs(title = "DCCO and BCNH over time",
       x = "Year",
       y = "Count")

# add management data ----
dcco_management <- read_csv("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/model datasets/dcco_management.csv") 
#read column titles
colnames(dcco_management)

#add a line for management using "management" column to indicate year that management started to both dcco and bcnh plotted together over time
ggplot() +
  geom_line(data = transform(dcco_data, year = as.numeric(as.character(year))), 
            aes(x = year, y = dcco_count, group=1, color = "DCCO")) +
  geom_line(data = transform(bcnh_data, year = as.numeric(as.character(year))), 
            aes(x = year, y = bcnh_count, group=1, color = "BCNH")) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2009, y = 600, 
           label = "Management starts", angle = 90, size = 2, vjust = -0.5) +
  scale_x_continuous(limits = c(1992, 2023)) +
  labs(title = "", x = "Year", y = "Count", color = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, vjust = 0.5),
        plot.margin = margin(b = 20))

#save as png
ggsave("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo/dcco_bcnh_over_time.png")

## calculate colony growth index of bcnh ----
#Formula: (current - previous) / max(current, previous)
#index ranges from -1 to 1, with -1 indicating complete abandonment, 0 indicating no change, and 1 indicating large increases
bcnh_data$growth_index <- c(NA, 
                            mapply(function(curr, prev) {
                              (curr - prev) / max(curr, prev)
                            },
                            bcnh_data$bcnh_count[-1],
                            bcnh_data$bcnh_count[-nrow(bcnh_data)]))


#calculate colony growth index of dcco, same as above ----
dcco_data$growth_index <- c(NA, 
                            mapply(function(curr, prev) {
                              (curr - prev) / max(curr, prev)
                            },
                            dcco_data$dcco_count[-1],
                            dcco_data$dcco_count[-nrow(dcco_data)]))


# plot both growth indexes together with vertical line at 2009 ----
ggplot() +
  geom_line(data = transform(dcco_data, year = as.numeric(as.character(year))), 
            aes(x = year, y = growth_index, group=1, color = "DCCO")) +
  geom_line(data = transform(bcnh_data, year = as.numeric(as.character(year))), 
            aes(x = year, y = growth_index, group=1, color = "BCNH")) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2009, y = 0.25, 
           label = "Management starts", angle = 90, size = 2, vjust = -0.5) +
  scale_x_continuous(limits = c(1992, 2023)) +
  labs(title = "",
       x = "Year", y = "Growth Index", color = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, vjust = 0.5),
        plot.margin = margin(b = 20))

#save as png
ggsave("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo/dcco_bcnh_growthindex_over_time.png")


#prep new combined_dataset.csv file with growth indexes updated ----
# Load combined_dataset.csv
combined_data<- read_csv("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo/combined_dataset.csv")
#select bcnh_growthindex and replace with growth_index columns from bcnh_data
combined_data$bcnh_growthindex <- bcnh_data$growth_index
#do the same for dcco_growthindex
combined_data$dcco_growthindex <- dcco_data$growth_index

#save as new combined_datasetJan2025.csv
write_csv(combined_data, "C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo/combined_datasetJan2025.csv")

