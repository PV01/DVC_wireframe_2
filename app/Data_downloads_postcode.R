library(sf)
library(dplyr)
library(leaflet)

# https://longair.net/blog/2021/08/23/open-data-gb-postcode-unit-boundaries/
# https://github.com/missinglink/uk-postcode-polygons/blob/master/geojson/B.geojson ### used this one

# https://www.doogal.co.uk/PostcodeDistricts

##############################################
# Contains OS data © Crown copyright and database right [year]
# Contains Royal Mail data © Royal Mail copyright and database right [year]
# Source: Office for National Statistics licensed under the Open Government Licence v.3.0
#######################


uk_postcodes_district<-st_read("app/data/UK postcode districts.geojson")


bham_postcodes <- uk_postcodes_district %>%
  filter(grepl("^B[0-9]{1,2}", name))


bham_postcodes <- uk_postcodes_district

leaflet(bham_postcodes) %>%
 addProviderTiles("CartoDB.Positron") %>%  # Light background map
  addCircleMarkers(
    radius = 5,
    color = "#3388ff",
    fillOpacity = 0.7,
    stroke = FALSE,
    label = ~name,
    labelOptions = labelOptions(
      textsize = "13px",
      direction = "auto",
      style = list("font-weight" = "bold")
    )
  )




#bham_postcodes <- st_read("app/data/B.geojson")


leaflet(bham_postcodes) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Light background map
  addCircleMarkers(
    radius = 5,
    color = "#3388ff",
    fillOpacity = 0.7,
    stroke = FALSE,
    label = ~name,
    labelOptions = labelOptions(
      textsize = "13px",
      direction = "auto",
      style = list("font-weight" = "bold")
    )
  )


######################################## color area code does not work

# Calculate area (in square meters)
bham_postcodes <- bham_postcodes %>%
  mutate(area_m2 = st_area(geometry))

# Create a color palette (smaller area = darker color)
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

