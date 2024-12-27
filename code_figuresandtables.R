library(readxl)
library(tidyr)
library(dplyr)
library(kableExtra)
library(webshot2)

#dec 20 didn't get PDF working, but HTML works. Open in browswer and screenshot.

# Read the Excel file
data <- read_excel("dcco_bcnh_database_ 1992_2023_july30th.xlsx", 
                   sheet = "Working Database 1992-2023")

# Create summary table
summary_table <- data %>%
  # First, select only the species columns
  select(starts_with(c("DCCO", "BCNH", "BCHN"))) %>%
  # Calculate the sum for each column
  summarise(across(everything(), ~sum(as.numeric(.), na.rm = TRUE))) %>%
  # Reshape from wide to long
  pivot_longer(
    everything(),
    names_to = "Year_Species",
    values_to = "Count"
  ) %>%
  # Extract year and species information
  mutate(
    Species = case_when(
      grepl("DCCO", Year_Species) ~ "Double-crested Cormorant",
      grepl("BCNH|BCHN", Year_Species) ~ "Black-crowned Night Heron"
    ),
    Year = as.numeric(substr(Year_Species, nchar(Year_Species)-3, nchar(Year_Species)))
  ) %>%
  select(Year, Species, Count) %>%
  arrange(Year, Species)

# Create wide format summary table
summary_wide <- summary_table %>%
  pivot_wider(
    names_from = Species,
    values_from = Count
  ) %>%
  arrange(Year)

# Print the summary table
print(summary_wide, n = Inf)

# Format the table for publication ----
publication_table <- summary_wide %>%
  # Rename columns for publication
  rename(
    "Year" = "Year",
    "Double-crested Cormorant" = "Double-crested Cormorant",
    "Black-crowned Night-Heron" = "Black-crowned Night Heron"  # Using proper hyphenation
  ) %>%
  # Format numbers with commas for thousands
  mutate(
    `Double-crested Cormorant` = format(`Double-crested Cormorant`, big.mark = ",", scientific = FALSE),
    `Black-crowned Night-Heron` = format(`Black-crowned Night-Heron`, big.mark = ",", scientific = FALSE)
  )

# Create the formatted table
table_output <- publication_table %>%
  kbl(
    caption = "Annual nest counts of Double-crested Cormorant and Black-crowned Night-Heron at Tommy Thompson Park, Toronto, Ontario (1992-2023).",
    align = c("c", "r", "r"),
    booktabs = TRUE
  ) %>%
  kable_classic(full_width = FALSE) %>%
  row_spec(0, bold = TRUE) %>%
  add_header_above(c(" " = 1, "Number of Nests" = 2)) %>%
  footnote(
    general = "Counts represent total number of active nests observed during breeding season surveys.",
    threeparttable = TRUE
  )

# Display the table
table_output

# Save as HTML
save_kable(table_output, "bird_nest_counts_table.html")

# table 3 ----
# Load required libraries
library(tidyr)
library(dplyr)
library(kableExtra)
library(webshot2)

# Assuming combined_data is already loaded
# Create summary data frame for each species by year
summary_data <- combined_data %>%
  select(year, 
         bcnh_nest_success, dcco_usurpation, raccoon_predation,
         winter_nestremoval, deterrence_activenestremoval, deterrence_night) %>%
  mutate(
    management = case_when(
      winter_nestremoval > 0 | deterrence_activenestremoval > 0 | deterrence_night > 0 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  summarise(
    # BCNH metrics
    bcnh_nest_success = sum(!is.na(bcnh_nest_success)),
    # DCCO metrics
    dcco_usurpation = sum(!is.na(dcco_usurpation)),
    # Shared metrics
    raccoon_presence = sum(!is.na(raccoon_predation)),
    management_years = sum(management)
  )

# Create the publication table
publication_table <- data.frame(
  Metric = c(
    "Years of Data",
    "Years with Nest Success Data",
    "Years with Usurpation Data",
    "Years with Raccoon Observations",
    "Years with Colony Management"
  ),
  "Double-crested Cormorant" = c(
    paste(min(combined_data$year), "-", max(combined_data$year)),
    "NA",
    as.character(summary_data$dcco_usurpation),
    as.character(summary_data$raccoon_presence),
    as.character(summary_data$management_years)
  ),
  "Black-crowned Night-Heron" = c(
    paste(min(combined_data$year), "-", max(combined_data$year)),
    as.character(summary_data$bcnh_nest_success),
    "NA",
    as.character(summary_data$raccoon_presence),
    as.character(summary_data$management_years)
  )
)

# Create the formatted table
table_output <- publication_table %>%
  kbl(
    caption = "Distribution of observations for Double-crested Cormorant and Black-crowned Night-Heron at Tommy Thompson Park, Toronto, Ontario (1992-2023).",
    align = c("l", "c", "c"),
    booktabs = TRUE
  ) %>%
  kable_classic(full_width = FALSE) %>%
  row_spec(0, bold = TRUE) %>%
  add_header_above(c(" " = 1, "Species" = 2)) %>%
  footnote(
    general = "Data represents years with recorded observations for each metric. NA indicates metric not applicable for that species.",
    threeparttable = TRUE
  )

# Display the table
table_output

# Save as HTML
save_kable(table_output, "species_distribution_table.html")