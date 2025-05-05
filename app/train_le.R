library(tidyverse)
library(sf)
library(purrr)
library(ggplot2)


routes <- read.csv("app/data/birmingham_routes_manual_v9.csv")

points_sf <- st_as_sf(routes, coords = c("lon", "lat"), crs = 4326)

lines_sf <- points_sf %>%
  arrange(route, order) %>%
  group_by(route) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


ggplot() +
  geom_sf(data = lines_sf, aes(color = route), size = 1.2, linewidth=2) +  # Lines
  geom_sf(data = points_sf, size = 2) +                        # Stations
  geom_text(data = points_sf,
            aes(label = station, geometry = geometry),
            stat = "sf_coordinates", hjust = -0.1, vjust = -0.5, size = 3) +
  scale_color_manual(values = c(
    "North" = "darkgreen",
    "South" = "firebrick",
    "East" = "steelblue",
    "West" = "goldenrod"
  )) +
theme_minimal() +
  labs(title = "Birmingham Rail Routes") +
theme_void()+
  theme(legend.position = "none")

########################

ggplot() +
  # Main route lines
  geom_sf(data = lines_sf, aes(color = route), size = 1.2, linewidth = 2) +
  
  # Curved connections (if you created `curved_routes`)
  geom_sf(data = curved_routes, aes(color = route), size = 1.5, linetype = "dashed") +
  
  # Station points in the same color as their route
  geom_sf(data = points_sf, aes(color = route), size = 3) +
  
  # Station labels — moved down & right
  geom_text(data = points_sf,
            aes(label = station, geometry = geometry, color = route),
            stat = "sf_coordinates", hjust = -0.6, vjust = 1.2, size = 3, show.legend = FALSE) +
  
  # Line colors
  scale_color_manual(values = c(
    "North" = "darkgreen",
    "South" = "firebrick",
    "East" = "steelblue",
    "West" = "goldenrod"
  )) +
  
  # Clean theme
  theme_minimal() +
  labs(title = "Birmingham Rail Routes with Color-Coded Stations") +
  theme_void() +
  theme(legend.position = "none")

########
ggplot() +
  # Route lines
  geom_sf(data = lines_sf, aes(color = route), size = 1.2, linewidth = 2) +
  
  # Station points colored by route
  geom_sf(data = points_sf, aes(color = route), size = 3) +
  
  # Station labels (manual, consistent offset)
  geom_text(
    data = points_sf,
    aes(label = station, geometry = geometry),
    stat = "sf_coordinates",
    hjust = -0.3,  # Rightward
    vjust = 0.25,   # Downward
    size = 3,
    color = "black",
    show.legend = FALSE
  ) +
  
  # Route colors
  scale_color_manual(values = c(
    "North" = "darkgreen",
    "South" = "firebrick",
    "East" = "steelblue",
    "West" = "goldenrod"
  )) +
  theme_minimal() +
  labs(title = "Birmingham Rail Routes") +
  theme_void() +
  theme(legend.position = "none")