library(dplyr)
library(sf)
library(leaflet)
library(leafgl)
library(rlang)
library(htmlwidgets)
library(jsonlite)
library(rmapshaper)

uk_le_data<-read_sf("data/uk_le_data.geojson")

uk_le_data <- ms_simplify(uk_le_data, keep = 0.5, keep_shapes = TRUE)