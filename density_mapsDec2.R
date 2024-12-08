#test one map, one block, one species ----

# Dec 4- fixed legend. added transparency to density map. tightened map extent.
# generated code for rest of maps; they show the same density/values for all maps- filtering issue?. 

# Library and load data ----
library(ggplot2)
library(tidyverse)
library(sf)
library(tmap)
library(grid)
library(png)
library(spatstat)
library(stars)

# Read and process data
locations_bcnhdcco <- read_csv("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/model datasets/arcgismerged_cleanedbcnhdcco.csv")

# Create time blocks, convert to sf object, create pen polygon ----
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

# Function to print data extent for debugging
print_data_extent <- function(data) {
  bbox <- st_bbox(data)
  cat("Data extent:\n")
  print(bbox)
}

#bbox_ttp <- c(
#  xmin = -79.35, # Western boundary
#  xmax = -79.32, # Eastern boundary
#  ymin = 43.61,  # Southern boundary
#  ymax = 43.64   # Northern boundary
#)

bbox_ttp <- c(
  xmin = -79.345, # Just west of -79.34367
  xmax = -79.336, # Just east of -79.33801
  ymin = 43.619,  # Just south of 43.62063
  ymax = 43.629   # Just north of 43.62758
)

# Modified create_test_map function
#filtered_data <- st_transform(filtered_data, 4326)
create_test_map <- function(filtered_data, species_name, year_block) {
  tmap_mode("plot")
  
  # Create window using our expanded bbox_ttp
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
    sigma = 0.00005,  # Adjusted smoothing parameter - adjust for tighter spread
    edge = FALSE,
    diggle = TRUE,
    positive = TRUE,
    dimyx = c(100, 100),
    kernel = "gaussian",
    units = "kilometers"
  )
  
  # After density_map creation in create_test_map function
  #density_map$v <- as.numeric(scale(density_map$v))  # Standardize values, broke the code
  
  density_stars <- st_as_stars(density_map)
  st_crs(density_stars) <- 4326
  st_crs(density_stars) <- st_crs(filtered_data)
  density_stars[[1]] <- density_stars[[1]] / 1000000  # Convert to km²
  #threshold <- max(density_stars[[1]], na.rm=TRUE)/50
  threshold = 10
  density_stars[[1]][density_stars[[1]] < threshold] <- NA
  names(density_stars) <- "density"
  
  # Calculate calculate the density values and breaks
  density_values <- as.vector(density_stars[[1]])
  density_values <- density_values[!is.na(density_values)]
  density_values <- density_values[density_values >= 10] # changed from >0 to >=10
  
  min_val <- min(density_values)
  max_val <- max(density_values)
# breaks <- pretty(density_values, n = 6)
  breaks <- seq(from = 10, to = max_val, length.out = 6)  # Forces exactly 6 breaks 
#  breaks <- breaks[breaks >= 10]  # Ensure all breaks are >= 10
  
  # print for debugging
  print("Density values range:")
  print(range(density_values, na.rm=TRUE))
  print("Breaks:")
  print(breaks)
  
  # Create map with modified settings
  
  # Create full species name
  species_fullname <- if(species_name == "BCNH") {
    "Black-crowned Night-Heron"
  } else {
    "Double-crested Cormorant"
  }
  
  # print detailed coordinate information
  coords <- st_coordinates(filtered_data)
  cat("\nCoordinate ranges:\n")
  cat("Longitude (x):", min(coords[,1]), "to", max(coords[,1]), "\n")
  cat("Latitude (y):", min(coords[,2]), "to", max(coords[,2]), "\n")
  
  # create test map
  tm <- tm_basemap(server = "CartoDB.Positron") +
    tm_shape(density_stars, bbox = bbox_ttp) +
    tm_raster(
      col = "density",
      palette = colorRampPalette(c("white", "#404040"))(100),
      breaks = breaks,
      #labels = format(round(breaks,2), scientific = FALSE),
      labels = paste0(round(breaks, 0), " nests/km²"),
      title = "Nest Density",
      value.na = "transparent",
      showNA = FALSE,
      col_alpha = 0.4, #change this value between 0.2 -0.5 to make density more transparent
      na.color = "transparent",
      interpolate = TRUE,
      tolerance = 0.1,  
      colorNA = "transparent", #do not remove or grey line appears
      border.col = "transparent" 
      )+
    tm_title(text = paste0(species_fullname, " Nest Density\n", year_block, "-", year_block + 4)) +
    tm_layout(
      legend.outside = TRUE,
      legend.outside.position = "right",
      frame = FALSE,
      asp = 1,
      bg.color = "white",
      inner.margins = c(0, 0, 0, 0)  # Remove internal margins
    )
  
  return(tm)
}

# Test settings ----
test_species <- "DCCO"  # or "BCNH"
test_year_block <- 1990  # Choose a year block that exists in your data

# Print available year blocks
cat("Available year blocks in the data:\n")
print(unique(species_sf$year_block))

# Process test case
cat("\nProcessing test case:", test_species, "for year block", test_year_block, "\n")

filtered_data <- species_sf %>%
  filter(species == test_species & 
           year_block == test_year_block & 
           count > 0) %>%
  filter(!is.na(count))

# Print number of points
cat("\nNumber of points:", nrow(filtered_data), "\n")

if(nrow(filtered_data) > 0) {
  filtered_data <- st_transform(filtered_data, 4326)
  
  tm <- create_test_map(filtered_data, test_species, test_year_block)
  
  # Save the test map
  tmap_save(tm, 
            filename = paste0("test_density_map_", test_species, "_", test_year_block, ".png"),
            width = 8, 
            height = 8,
            units = "in", 
            dpi = 300)
  
  # Display the map
  print(tm)
  
  cat("\nTest map has been saved as: test_density_map_", test_species, "_", test_year_block, ".png\n", sep="")
} else {
  cat("\nNo data found for", test_species, "in year block", test_year_block, "\n")
}

#generate all maps test Dec 5th ----

# Get unique combinations of species and year blocks
species_list <- unique(species_sf$species)
year_blocks <- unique(species_sf$year_block)

# Create output directory if it doesn't exist
dir.create("density_maps", showWarnings = FALSE)

# Loop through each combination and create maps
for(current_species in species_list) {
  for(current_year_block in year_blocks) {
    cat("\nProcessing:", current_species, "for year block", current_year_block, "\n")
    
    # Filter data using same logic as test case
    filtered_data <- species_sf %>%
      filter(species == current_species & 
               year_block == current_year_block & 
               count > 0) %>%
      filter(!is.na(count))
    
    # Only create map if we have data
    if(nrow(filtered_data) > 0) {
      filtered_data <- st_transform(filtered_data, 4326)
      
      # Create map using existing function
      tm <- create_test_map(filtered_data, current_species, current_year_block)
      
      # Save map with organized naming
      filename <- file.path("density_maps", 
                            paste0("density_map_", 
                                   current_species, "_", 
                                   current_year_block, 
                                   ".png"))
      
      # Save the map
      tmap_save(tm, 
                filename = filename,
                width = 8, 
                height = 8,
                units = "in", 
                dpi = 300)
      
      cat("Saved map:", filename, "\n")
    } else {
      cat("No data found for", current_species, 
          "in year block", current_year_block, "\n")
    }
  }
}

# Print summary of processing
cat("\nProcessing complete!\n")
cat("Total species processed:", length(species_list), "\n")
cat("Total year blocks processed:", length(year_blocks), "\n")
cat("Maps saved in the 'density_maps' directory\n")

#combine all maps and save in grid ----

# libraries
library(gridExtra)
library(grid)
library(ggplot2)

# Modify the plotting section
create_map_grid <- function() {
  # Get list of all generated maps
  map_files <- list.files("density_maps", pattern = "density_map_.*\\.png$", full.names = TRUE)
  
  # Split files by species
  bcnh_files <- sort(map_files[grep("BCNH", map_files)])
  dcco_files <- sort(map_files[grep("DCCO", map_files)])
  
  # Read all PNG files and convert to grobs
  bcnh_grobs <- lapply(bcnh_files, function(f) {
    img <- png::readPNG(f)
    rasterGrob(img, interpolate = TRUE)
  })
  
  dcco_grobs <- lapply(dcco_files, function(f) {
    img <- png::readPNG(f)
    rasterGrob(img, interpolate = TRUE)
  })
  
  # Calculate dimensions
  n_years <- length(bcnh_grobs)
  n_rows_per_page <- 3  # Adjust this number to control maps per page
  n_pages <- ceiling(n_years / n_rows_per_page)
  
  # Create multi-page PDF
  pdf("combined_density_maps.pdf", width = 11, height = 8.5)  # Landscape orientation
  
  # Create pages
  for(page in 1:n_pages) {
    grid.newpage()
    
    # Calculate rows for this page
    start_idx <- (page-1) * n_rows_per_page + 1
    end_idx <- min(page * n_rows_per_page, n_years)
    current_rows <- end_idx - start_idx + 1
    
    # Create layout
    pushViewport(viewport(layout = grid.layout(current_rows, 2)))
    
    # Plot maps for this page
    for(i in 1:current_rows) {
      idx <- start_idx + i - 1
      
      # Plot BCNH
      if(idx <= length(bcnh_grobs)) {
        pushViewport(viewport(layout.pos.row = i, layout.pos.col = 1))
        grid.draw(bcnh_grobs[[idx]])
        popViewport()
      }
      
      # Plot DCCO
      if(idx <= length(dcco_grobs)) {
        pushViewport(viewport(layout.pos.row = i, layout.pos.col = 2))
        grid.draw(dcco_grobs[[idx]])
        popViewport()
      }
    }
  }
  
  dev.off()
  
  cat("\nGrid layout created!\n")
  cat("- PDF saved with", n_pages, "pages\n")
  cat("- Each page has", n_rows_per_page, "rows (or fewer on last page)\n")
}

# Run the function
create_map_grid()
