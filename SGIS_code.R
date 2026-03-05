install.packages("rgbif")
install.packages("sf")
install.packages("terra")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggspatial")
install.packages("leaflet")
install.packages("maps")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rnaturalearthhires")

library(rgbif)
library(maps)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
library(maps)

sa <- ne_countries(country = "South Africa", returnclass = "sf")

ggplot() +
  geom_sf(data = sa, fill = "gray", color = "black") +
  coord_sf(
    xlim = c(16, 33),   # longitude range
    ylim = c(-36, -27), # latitude range
    expand = FALSE
  ) +
  theme_minimal() +
  labs(title = "South African Coastline")

# Plotting multiple species 

species <- c(
  "Carcharodon carcharias",
  "Haploblepharus pictus",
  "Acroteriobatus annulatus",
  "Torpedo sinuspersici"
)

get_species_data <- function(sp){
  
  key <- name_backbone(name = sp)$speciesKey
  
  occ <- occ_search(
    taxonKey = key,
    hasCoordinate = TRUE,
    country = "ZA", 
    limit = 2000
  )
  
  data <- occ$data %>%
    select(species, decimalLongitude, decimalLatitude) %>%
    na.omit()
  
  return(data)
}

shark_data <- bind_rows(lapply(species, get_species_data))

shark_sf <- st_as_sf(
  shark_data,
  coords = c("decimalLongitude","decimalLatitude"),
  crs = 4326
)

st_crs(shark_sf)

geom_sf(data = shark_sf, aes(color = species), size = 2) +
  scale_color_manual(values = c(
    "Carcharodon carcharias" = "red",
    "Haploblepharus pictus" = "blue",
    "Acroteriobatus annulatus" = "yellow",
    "Torpedo sinuspersici" = "purple"
  ))

ggplot() +
  geom_sf(data = sa, fill = "gray", color = "black") +   # base map
  geom_sf(data = shark_sf, aes(color = species), size = 2, alpha = 0.7) +  # points
  coord_sf(xlim = c(16, 33), ylim = c(-37, -27), expand = FALSE) +  # zoom to coastline
  theme_minimal(base_size = 14) +
  labs(
    title = "Shark Occurrences in South Africa",
    color = "Species"
  )





