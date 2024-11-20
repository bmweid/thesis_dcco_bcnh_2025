library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)

# this code works as a test with leaflet to visualize both species on one map in ttp. not yet density maps, just testing
#visualizing

locations_bcnhdcco <- read_csv("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/model datasets/arcgismerged_cleanedbcnhdcco.csv")

# remove records with NA locations or zero counts for species per location per year
# gather by species, location- treeid?, & year

long_data <- locations_bcnhdcco |> 
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
    treeid, year, xcoord, ycoord, tag, peninsula, species, count
  ) |> 
  arrange(treeid, year, species)

# mapdata <- map_data("world")
# view(mapdata)

species_sf <- long_data %>%
  # Convert to sf object with original coordinates
  st_as_sf(coords = c("xcoord", "ycoord"), 
           crs = 26917) %>%  # UTM Zone 17N (common for Toronto area)
  # Transform to WGS84 for mapping
  st_transform(crs = 4326)

toronto_bbox <- st_bbox (c(xmin = -79.355, 
                           xmax = -79.330,
                           ymin = 43.620, 
                           ymax = 43.640))

(# Create the map with proper basemap
ggplot() +
  # Add OpenStreetMap basemap
  annotation_map_tile(type = "osm", zoom = 15, progress = "none") +
  # Add species points
  geom_sf(data = species_sf, 
          aes(color = species), 
          size = 3,
          alpha = 0.7) +
  # Set color scheme
  scale_color_manual(values = c("BCNH" = "purple", "DCCO" = "yellow")) +
  # Add map elements
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", 
                         padding = unit(c(0.2, 0.2), "cm")) +
  # Focus map on Tommy Thompson Peninsula
  coord_sf(xlim = c(-79.355, -79.330),
           ylim = c(43.620, 43.640)) +
  # Customize theme
  theme_minimal() +
  labs(title = "Species Locations at Tommy Thompson Peninsula",
       subtitle = "Toronto, Ontario",
       color = "Species"))

# If the above still doesn't work, here's an alternative using leaflet
leaflet(species_sf) %>%
  addTiles() %>%  # Add OpenStreetMap tiles
  addCircleMarkers(
    color = ~ifelse(species == "BCNH", "purple", "yellow"),
    radius = 6,
    fillOpacity = 0.7,
    stroke = FALSE,
    label = ~species
  ) %>%
  addLegend(
    position = "topright",
    colors = c("purple", "yellow"),
    labels = c("BCNH", "DCCO"),
    title = "Species"
  )