library(archive)
library(sf)
library(dplyr)
library(leaflet)

# https://longair.net/blog/2021/08/23/open-data-gb-postcode-unit-boundaries/

#You may use this data under the Open Government License v3.0, and must include the following copyright notices:
#Contains OS data © Crown copyright and database right 2020
#Contains Royal Mail data © Royal Mail copyright and database right 2020
#Source: Office for National Statistics licensed under the Open Government Licence v.3.0


https://github.com/missinglink/uk-postcode-polygons/blob/master/geojson/B.geojson

#List files
archive::archive("app/data/gb-postcodes-v5.tar.bz2")

# Extract all files
archive::archive_extract("app/data/gb-postcodes-v5.tar.bz2", dir = "app/data/postcodes")



# Load it
bham_postcodes <- st_read("app/data/B.geojson")


################################################### code to debug
# Step 2: Read them all into a list
geojson_list <- lapply(geojson_files, st_read)

# Step 3: Combine all into a single sf object
all_postcodes <- do.call(rbind, geojson_list)

# Step 4: Inspect column names
names(all_postcodes)

# Step 5: Filter only Birmingham postcodes
# Assume postcode field is "pcd" (change it if different)
bham_postcodes <- all_postcodes %>%
  filter(grepl("^B", pcd))  # B1, B2, B15 etc.

# Step 6: (Optional) Save combined Birmingham GeoJSON
st_write(bham_postcodes, "birmingham_postcodes.geojson", driver = "GeoJSON")
##########################





















# Quick plot
plot(postcode_districts)

library(sf)
library(dplyr)
library(leaflet)

# Step 1: Load your postcode district polygons
# (Assume already loaded like this)
# postcode_districts <- st_read("extracted_folder/your_file.geojson")

# Step 2: Filter Birmingham postcode districts (B prefix)
bham_postcodes <- postcode_districts %>%
  filter(grepl("^B", your_postcode_field_name))  # Replace with actual field name!

# Step 3: Plot using Leaflet
leaflet(bham_postcodes) %>%
  addProviderTiles("CartoDB.Positron") %>%   # Light background map
  addPolygons(
    color = "#3388ff",
    weight = 1,
    fillOpacity = 0.3,
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ),
    label = ~your_postcode_field_name,  # Replace with actual field name!
    labelOptions = labelOptions(
      textsize = "13px",
      direction = "auto",
      style = list("font-weight" = "bold")
    )
  )

names(postcode_districts)

bham_postcodes <- postcode_districts %>%
  filter(grepl("^B", pcd))

label = ~pcd



bham_postcodes <- postcode_districts %>%
  filter(grepl("^B", pcd))

leaflet(bham_postcodes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    color = "#3388ff",
    weight = 1,
    fillOpacity = 0.3,
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ),
    label = ~pcd,
    labelOptions = labelOptions(
      textsize = "13px",
      direction = "auto"
    )
  )



######################################## color area


# Step 2: Calculate area (in square meters)
bham_postcodes <- bham_postcodes %>%
  mutate(area_m2 = st_area(geometry))

# Step 3: Create a color palette (smaller area = darker color)
pal <- colorNumeric(
  palette = "YlOrRd",   # Yellow-Orange-Red color scale
  domain = as.numeric(bham_postcodes$area_m2),
  reverse = TRUE        # Reverse so small areas are darker
)

# Step 4: Plot in Leaflet
leaflet(bham_postcodes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(as.numeric(area_m2)),
    color = "#444444",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ),
    label = ~paste0(pcd, "<br>Area: ", round(as.numeric(area_m2)/1e6, 2), " km²"),
    labelOptions = labelOptions(
      textsize = "13px",
      direction = "auto",
      style = list("font-weight" = "bold")
    )
  ) %>%
  addLegend(
    pal = pal,
    values = as.numeric(bham_postcodes$area_m2),
    title = "Area (m²)",
    position = "bottomright"
  )


