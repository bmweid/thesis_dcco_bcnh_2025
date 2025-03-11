library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)


#file: Jan 2025 data check and re-calculation
#loads data for both species
#plots against management activities commencing
#re-calculates colony growth index

setwd("~/thesis_dcco_bcnh_2025/Final Files Jan 2025/2018 Updates")

#load bcnh and dcco data
bcnh_data <- read.csv("bcnh_data.csv")
dcco_data <- read.csv("dcco_data.csv")

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

#print 2018 growth index values for each species
print(bcnh_data$growth_index[bcnh_data$year == 2018])
print(dcco_data$growth_index[dcco_data$year == 2018])

#plot growth index of bcnh and dcco together
ggplot() +
  geom_line(data = bcnh_data, aes(x = year, y = growth_index, group=1, color = "BCNH")) +
  geom_line(data = dcco_data, aes(x = year, y = growth_index, group=1, color = "DCCO")) +
  labs(title = "BCNH and DCCO Growth Index over time",
       x = "Year",
       y = "Growth Index")


#prep new combined_dataset.csv file with growth indexes updated ----
# Load combined_dataset.csv
combined_dataset <- read.csv("combined_datasetMarch2025.csv")
#select bcnh_growthindex and replace with growth_index columns from bcnh_data
combined_dataset$bcnh_growthindex <- bcnh_data$growth_index
#do the same for dcco_growthindex
combined_dataset$dcco_growthindex <- dcco_data$growth_index
#save changes to existing csv
write.csv(combined_dataset, "combined_datasetMarch2025.csv", row.names = FALSE)

#check 2018 for dcco and bcnh in combined dataset
combined_dataset_2018 <- combined_dataset %>% filter(year == 2018)
print(combined_dataset_2018)
