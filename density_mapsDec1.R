#dec 1 - getting close. need to fix map extent, and switch to greyscale, 
#double check that points with no count data are excluded
#then generate for all years

#library and load data ----
library(ggplot2)
library(tidyverse)
library(sf)
library(tmap)
library(grid)
library(png)
library(spatstat)
library(stars)

# Read and process data (keeping your existing data processing code)
locations_bcnhdcco <- read_csv("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/model datasets/arcgismerged_cleanedbcnhdcco.csv")


# create time blocks, convert to sf object, create pen polygon ----

# Create 5-year blocks and process data
long_data <- locations_bcnhdcco |> 
  mutate(
    year_block = 5 * floor(year/5),
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
  )

# Convert to sf object
species_sf <- long_data %>%
  st_as_sf(coords = c("xcoord", "ycoord"), 
           crs = 26917) %>%
  st_transform(crs = 4326)

# First, let's print the current extent to debug
print_data_extent <- function(data) {
  bbox <- st_bbox(data)
  cat("Data extent:\n")
  print(bbox)
}

# Set precise extent for Tommy Thompson Park
bbox_ttp <- c(
  xmin = -79.3485, # Western edge
  ymin = 43.6220,  # Southern edge
  xmax = -79.3320, # Eastern edge
  ymax = 43.6340   # Northern edge
)

# Modified create_test_map function
create_test_map <- function(filtered_data, species_name, year_block) {
  tmap_mode("plot")
  
  # Create window using precise coordinates
  window <- owin(
    xrange = c(bbox_ttp["xmin"], bbox_ttp["xmax"]),
    yrange = c(bbox_ttp["ymin"], bbox_ttp["ymax"])
  )
  
  coords <- st_coordinates(filtered_data)
  pts <- ppp(
    x = coords[,1], 
    y = coords[,2], 
    window = window,
    marks = filtered_data$count
  )
  
  density_map <- density.ppp(
    pts, 
    weights = pts$marks,
    sigma = 0.0005,
    edge = TRUE,
    positive = TRUE
  )
  
  density_stars <- st_as_stars(density_map)
  st_crs(density_stars) <- st_crs(filtered_data)
  
  # Calculate breaks
  density_values <- as.vector(density_stars[[1]])
  density_values <- density_values[!is.na(density_values)]
  density_values <- density_values[density_values > 0]
  
  min_val <- min(density_values)
  max_val <- max(density_values)
  breaks <- seq(min_val, max_val, length.out = 7)
  
  # Create map with fixed bbox
  tm <- tm_basemap(server = "CartoDB.Positron") +
    tm_shape(density_stars, bbox = bbox_ttp) +
    tm_raster(
      col.scale = tm_scale_intervals(
        values = "Blues",
        breaks = breaks
      ),
      col.legend = tm_legend(
        title = paste(species_name, "\nNest Density\n", 
                      year_block, "-", year_block + 4),
        format = function(x) format(x, scientific = TRUE, digits = 2)
      ),
      col_alpha = 0.7
    ) +
    tm_title(text = paste(species_name, "Nests", year_block, "-", year_block + 4)) +
    tm_layout(
      legend.outside = TRUE,
      legend.outside.position = "right",
      frame = FALSE,
      asp = 1,
      bg.color = "white"
    ) +
    tm_view(bbox = bbox_ttp)  # Force the view to use our precise bbox
  
  return(tm)
}

# Main loop for processing and saving maps
for(species_name in c("BCNH", "DCCO")) {
  cat("\nProcessing", species_name, "\n")
  
  filtered_data <- species_sf %>%
    filter(species == species_name & 
             year_block == test_year_block & 
             count > 0) %>%
    filter(!is.na(count))
  
  # Print extent information for debugging
  cat("\nFiltered data extent:\n")
  print_data_extent(filtered_data)
  
  if(nrow(filtered_data) > 0) {
    filtered_data <- st_transform(filtered_data, 4326)
    
    tm <- create_test_map(filtered_data, species_name, test_year_block)
    
    tmap_save(tm, 
              filename = paste0("test_map_", species_name, "_", test_year_block, ".png"),
              width = 8, 
              height = 8,
              units = "in", 
              dpi = 300)
    
    print(tm)
  }
}
#run the rest of the maps ----

# Get unique year blocks
year_blocks <- unique(species_sf$year_block)

# Create directory for output if it doesn't exist
dir.create("map_outputs", showWarnings = FALSE)

# Create PDF with all maps
pdf("colony_distribution_by_year.pdf", width = 11, height = 8.5)

# Set up the layout for two maps per page
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))

for(year_block in year_blocks) {
  for(species_name in c("BCNH", "DCCO")) {
    # Filter data
    filtered_data <- species_sf %>%
      filter(species == species_name & 
               year_block == year_block & 
               count > 0) %>%
      filter(!st_intersects(., shoreline, sparse = FALSE)[,1] | 
               st_intersects(., buffer_wgs84, sparse = FALSE)[,1])
    
    # Create and save map
    tm <- create_map(filtered_data, species_name, year_block)
    
    # Save individual PNG if needed
    tmap_save(tm, 
              filename = file.path("map_outputs", 
                                   paste0("map_", species_name, "_", year_block, ".png")),
              width = 8, 
              height = 8, 
              units = "in", 
              dpi = 300)
    
    # Add to PDF
    print(tm)
  }
}

dev.off()