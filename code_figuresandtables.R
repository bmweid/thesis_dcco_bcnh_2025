library(readxl)
library(tidyr)
library(dplyr)
library(kableExtra)
library(webshot2)
library(ggplot2)  # Add this library for plotting
library(scales)   # Add this for better axis formatting

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
    caption = "Table 1: Annual nest counts of Double-crested Cormorant and Black-crowned Night-Heron at Tommy Thompson Park, Toronto, Ontario (1992-2023).",
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


# Create the population trend plot ----

library(stringr)

# Create the caption with manual width
caption_text <- str_wrap("Figure 4: Population trends of Double-crested Cormorant and Black-crowned Night-Heron nests at Tommy Thompson Park, Toronto, Ontario (1992-2023). Each point represents the total number of active nests observed during breeding season surveys.", width = 100)

# Use in the plot
population_plot <- ggplot(summary_table, aes(x = Year, y = Count, color = Species)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("Double-crested Cormorant" = "#1b9e77", 
                                "Black-crowned Night Heron" = "#d95f02")) +
  labs(x = "Year",
       y = "Number of Nests",
       color = "Species",
       caption = caption_text) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(
      hjust = 0, 
      size = 10,
      lineheight = 1.2
    ),
    plot.margin = margin(t = 10, r = 30, b = 30, l = 10, unit = "pt")
  )

# Save as PDF with adjusted dimensions
ggsave("Figure4_bird_populations.pdf", population_plot, 
       width = 8, height = 7, units = "in", device = "pdf")

# Save as PNG with high resolution
ggsave("Figure4_bird_populations.png", population_plot, 
       width = 8, height = 7, units = "in", dpi = 300)