library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(leaflet.extras)  # For heatmap functionality

#nov 18: maps are saving as blank pdfs. data vis needs work- single colour scale, reduce points?

# Read and process data
locations_bcnhdcco <- read_csv("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/model datasets/arcgismerged_cleanedbcnhdcco.csv")

# Create 5-year blocks and process data
long_data <- locations_bcnhdcco |> 
  mutate(
    year_block = 5 * floor(year/5),  # Create 5-year blocks
    year_block_label = paste(year_block, year_block + 4, sep="-")
  ) |>
  pivot_longer(
    cols = c(bcnh_count, dcco_count),
    names_to = "species",
    values_to = "count"
  ) |> 
  mutate(
    species = case_when(
      species == "bcnh_count" ~ "BCNH",
      species == "dcco_count" ~ "DCCO",
      TRUE ~ species
    )
  ) |> 
  select(
    treeid, year, year_block, year_block_label, xcoord, ycoord, 
    tag, peninsula, species, count
  ) |> 
  arrange(treeid, year, species)

# Convert to sf object
species_sf <- long_data %>%
  st_as_sf(coords = c("xcoord", "ycoord"), 
           crs = 26917) %>%
  st_transform(crs = 4326)

# Define Toronto bbox
toronto_bbox <- st_bbox(c(xmin = -79.355, 
                          xmax = -79.330,
                          ymin = 43.620, 
                          ymax = 43.640))

# Function to create density map for a specific species and time period
create_density_map <- function(data, species_name, year_block) {
  filtered_data <- data %>%
    filter(species == species_name & year_block == !!year_block)
  
  # Get the year block label
  year_block_label <- paste(year_block, year_block + 4, sep="-")
  
  # Calculate weight based on count
  max_count <- max(filtered_data$count, na.rm = TRUE)
  weighted_data <- filtered_data %>%
    mutate(weight = count / max_count)
  
  # Create the map
  m <- leaflet(weighted_data) %>%
    addTiles() %>%
    addHeatmap(
      radius = 20,
      blur = 15,
      intensity = ~weight,
      max = 1.0
    ) %>%
    addCircleMarkers(
      radius = ~sqrt(count)/2,
      color = ifelse(species_name == "BCNH", "purple", "yellow"),
      fillOpacity = 0.7,
      stroke = FALSE,
      label = ~paste("Count:", count)
    ) %>%
    addLegend(
      position = "topright",
      colors = ifelse(species_name == "BCNH", "purple", "yellow"),
      labels = species_name,
      title = paste("Species Density Map", year_block_label)
    )
  
  return(m)
}

# Get unique year blocks and species
year_blocks <- unique(species_sf$year_block)
species_list <- unique(species_sf$species)

# Create and save maps for each species and time period
for(year_block in year_blocks) {
  for(species_name in species_list) {
    print(paste("Creating map for", species_name, "year block:", year_block))
    map <- create_density_map(species_sf, species_name, year_block)
    
    # Save map (you'll need to modify the path)
    mapview::mapshot(
      map,
      file = paste0(
        "C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/heatmaps/maps/",  # Your desired path here
        species_name, "_",
        year_block,
        "_density.html"
      )
    )
  }
}

# Function to display all maps for a specific species
display_species_maps <- function(data, species_name) {
  year_blocks <- unique(data$year_block)
  
  for(year_block in year_blocks) {
    map <- create_density_map(data, species_name, year_block)
    print(paste("Displaying map for", species_name, "during", year_block))
    print(map)
    Sys.sleep(2)  # Pause between maps
  }
}

# Display maps for each species
for(species_name in species_list) {
  display_species_maps(species_sf, species_name)
}


## new chunk to try saving

# Retry saving maps as PDFs
library(webshot2)  

year_blocks <- unique(species_sf$year_block)
species_list <- unique(species_sf$species)

# Create directory if it doesn't exist
save_dir <- "C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/heatmaps/maps/"
dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

# Retry creating and saving maps
for(year_block in year_blocks) {
  for(species_name in species_list) {
    print(paste("Saving map for", species_name, "year block:", year_block))
    
    # Create the map
    map <- create_density_map(species_sf, species_name, year_block)
    
    # Create temporary HTML file
    temp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(map, temp_html, selfcontained = TRUE)
    
    # Create PDF filename
    pdf_filename <- file.path(save_dir, 
                              paste0(species_name, "_", 
                                     year_block, 
                                     "_density.pdf"))
    
    # Try saving PDF with error handling
    tryCatch({
      webshot2::webshot(
        url = temp_html,
        file = pdf_filename,
        delay = 0.5,
        zoom = 2  # Increase resolution
      )
      print(paste("Successfully saved:", pdf_filename))
    }, error = function(e) {
      print(paste("Error saving:", pdf_filename))
      print(e)
    })
    
    # Clean up temporary file
    unlink(temp_html)
    
    # Add small delay between saves
    Sys.sleep(2)
  }
}

# Print all files in the directory to verify saves
pdf_files <- list.files(save_dir, pattern = "*.pdf")
print("Saved PDF files:")
print(pdf_files)