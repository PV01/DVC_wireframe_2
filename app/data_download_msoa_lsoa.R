#### download msoa and lsoa

# Birmingham City Observatory: 
## https://cityobservatory.birmingham.gov.uk/explore/dataset/boundaries-msoa-wmca/information/?utm_source=chatgpt.com&disjunctive.local_authority_name&disjunctive.local_authority_code&disjunctive.msoa21nm&disjunctive.msoa21cd

# ArcGIS
# https://www.arcgis.com/home/item.html?id=d1363807d5594110ad0bcb37f11cb02e&sublayer=10&utm_source=chatgpt.com

# https://cityobservatory.birmingham.gov.uk/pages/home/
# https://fingertips.phe.org.uk/profiles

##########################################

library(dplyr)
library(sf)
library(leaflet)

wm_msoa<-read_sf("app/data/boundaries-msoa-wmca.geojson")
bham_msoa<-read_sf("app/data/boundaries-msoa-wmca_selected.geojson")
  
wm_lsoa<-read_sf("app/data/boundaries-lsoa-wmca.geojson")
bham_lsoa<-read_sf("app/data/boundaries-lsoa-wmca_selected.geojson")

bham_boundary <- st_union(bham_lsoa)
bham_boundary_sf <- st_sf(geometry = bham_boundary)
st_write(bham_boundary_sf,"app/data/bham_boundary.geojson", driver = "GeoJSON")




bham_postcodes<-read.csv("app/data/postcodes-latest-wmca.csv") %>%
                     filter(Local.Authority.Name=="Birmingham")
                   

bham_deprivation<-read.csv("app/data/deprivation-2019-all-indices-birmingham-postcodes.csv")


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addTiles(urlTemplate = "https://{s}.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png",
           attribution = "© OpenRailwayMap contributors") %>%
  setView(lng = -1.8904, lat = 52.4862, zoom = 10) %>%
  #addPolylines(data = rail_lines, color = "blue", weight = 2, opacity = 0.8) %>%
 # addCircleMarkers(data = stations,
 #                  radius = 4,
  #                 fillColor = "red",
   #                color = "white",
 #                  weight = 1,
   #                fillOpacity = 0.9,
   #                popup = ~name)




###################

world <- st_polygon(list(rbind(
  c(-180, -90), c(180, -90), c(180, 90), c(-180, 90), c(-180, -90)
))) |> st_sfc(crs = 4326)

# Subtract the region from the world to create a mask
boundary <- st_read("bham_boundary.geojson") |> st_transform(4326)
world <- st_polygon(list(rbind(
  c(-180, -90), c(180, -90), c(180, 90), c(-180, 90), c(-180, -90)
))) |> st_sfc(crs = 4326)
mask <- st_difference(world, st_union(boundary))


leaflet(bham_lsoa) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addTiles(urlTemplate = "https://{s}.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png",
           attribution = "© OpenRailwayMap contributors") %>% # nice light background
  setView(lng = -1.8904, lat = 52.4862, zoom = 11)%>%
  addPolygons(
    color = "#3388ff",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.3,
    highlightOptions = highlightOptions(
      color = "white",
      weight = 3,
      bringToFront = TRUE
    ),
    label = ~lsoa21nm,  # <-- label on hover
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addPolygons(data = mask,
              fillColor = "white",
              fillOpacity = 0.8,
              color = NA)

###########################
boundary <- st_read("bham_boundary.geojson") |> 
  st_transform(4326) |> 
  st_make_valid()

world <- st_as_sfc(st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = 90), crs = st_crs(4326))) |>
  st_cast("POLYGON")

# 3. Subtract boundary from world to make a 'hole'
mask_geom <- st_difference(world, st_union(boundary)) |> st_make_valid()

# 4. Leaflet map with layered panes
leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addMapPane("tiles", zIndex = 410) %>%
  addMapPane("mask", zIndex = 420) %>%
  addMapPane("boundary", zIndex = 430) %>%
  
  addTiles(
    urlTemplate = "https://{s}.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png",
    attribution = "© OpenRailwayMap contributors",
    options = pathOptions(pane = "tiles")
  ) %>%
  
  setView(lng = -1.8904, lat = 52.4862, zoom = 10) %>%
  
  # ✅ Add white polygon with a HOLE where West Midlands is
  addPolygons(data = mask_geom,
              fillColor = "white",
              fillOpacity = 1,
              color = NA,
              stroke = FALSE,
              options = pathOptions(pane = "mask")) %>%
  
  # Optional: add outline
  addPolygons(data = boundary,
              color = "red",
              weight = 2,
              fill = FALSE,
              options = pathOptions(pane = "boundary"))










#### LSOA to postcode

# Download the ONS Postcode Directory (or filtered for Birmingham)

# Sample simplified logic:

library(readr)
library(dplyr)

# Assume you have ONSPD data (postcode, LSOA, postcode district)
# Example structure
# pc6 = full postcode (e.g. B1 1AA)
# pcds = postcode district (e.g. B1)
# lsoa21cd = LSOA code (matching your polygons)


ons_lookup <- read_csv("app/data/NSPL21_NOV_2024_UK.csv")

head(ons_lookup)

bham_postcodes <- ons_lookup %>%
                      filter(lsoa21 %in% bham_lsoa$lsoa21cd)

nham_postcodes <- bham_postcodes %>%
  mutate(postcode_district = gsub("([A-Z0-9]+) .*", "\\1", pcds)) # keep part before space


# For each LSOA, get most common district
postcode_lsoa_summary <- bham_postcodes %>%
  group_by(lsoa21cd) %>%
  summarise(most_common_postcode = names(sort(table(postcode_district), decreasing = TRUE))[1])

bham_lsoa <- bham_lsoa %>%
  left_join(postcode_lsoa_summary, by = c("lsoa21cd" = "lsoa21cd"))

leaflet(bham_lsoa) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    color = "#3388ff",
    weight = 1,
    fillOpacity = 0.3,
    highlightOptions = highlightOptions(
      color = "white",
      weight = 3,
      bringToFront = TRUE
    ),
    label = ~most_common_postcode,
    labelOptions = labelOptions(
      textsize = "13px",
      direction = "auto"
    )
  )

#######################################
# Option 2: Directly get postcode areas (GeoJSON)
# There are also ready-made GeoJSON

# Doogal.co.uk
# Open Geography Portal


# Load postcode district GeoJSON
postcode_districts <- st_read("postcode_districts.geojson")

# Filter for Birmingham postcode districts
bham_postcode_districts <- postcode_districts %>%
  filter(grepl("^B", pcd))

# Leaflet
leaflet(bham_postcode_districts) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    color = "#ff6600",
    weight = 1,
    fillOpacity = 0.3,
    label = ~pcd,  # pcd is postcode district like B1, B2, B15
    labelOptions = labelOptions(
      textsize = "13px",
      direction = "auto"
    )
  )


############################


library(sf)
library(leaflet)

# Load boundary and convert CRS
boundary <- st_read("boundary.geojson") |> st_transform(4326)

# Load rail lines and stations as sf objects
rail_lines <- st_read("rail_lines.geojson") |> st_transform(4326)
stations <- st_read("stations.geojson") |> st_transform(4326)

# Clip to boundary
rail_lines_clipped <- st_intersection(rail_lines, boundary)
stations_clipped <- st_intersection(stations, boundary)

# Create Leaflet map
leaflet() %>%
  addTiles(urlTemplate = "https://{s}.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png",
           attribution = "© OpenRailwayMap contributors") %>%
  setView(lng = -1.8904, lat = 52.4862, zoom = 10) %>%
  addPolylines(data = rail_lines_clipped, color = "blue", weight = 2) %>%
  addCircleMarkers(data = stations_clipped,
                   radius = 4,
                   fillColor = "red",
                   color = "white",
                   weight = 1,
                   fillOpacity = 0.9,
                   popup = ~name)


bham_boundary <- st_read("app/data/bham_boundary.geojson")
# Load rail lines and stations as sf objects
rail_lines <- st_read("rail_lines.geojson") |> st_transform(4326)
stations <- st_read("stations.geojson") |> st_transform(4326)

# Clip to boundary
rail_lines_clipped <- st_intersection(rail_lines, boundary)
stations_clipped <- st_intersection(stations, boundary)

# Create Leaflet map
leaflet() %>%
  addTiles(urlTemplate = "https://{s}.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png",
           attribution = "© OpenRailwayMap contributors") %>%
  setView(lng = -1.8904, lat = 52.4862, zoom = 10) %>%
  addPolylines(data = rail_lines_clipped, color = "blue", weight = 2) %>%
  addCircleMarkers(data = stations_clipped,
                   radius = 4,
                   fillColor = "red",
                   color = "white",
                   weight = 1,
                   fillOpacity = 0.9,
                   popup = ~name)

leaflet() %>%
  addTiles(urlTemplate = "https://{s}.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png",
           attribution = "© OpenRailwayMap contributors") %>%
  setView(lng = -1.8904, lat = 52.4862, zoom = 10) %>%
  addPolygons(data = bham_boundary,
              color = "black",
              fillOpacity = 0.1,
              weight = 2,
              popup = "West Midlands")


bham_boundary <- st_read("west_midlands_boundary.geojson") |> st_transform(4326)

# Create a big square polygon covering the world
world <- st_polygon(list(rbind(
  c(-180, -90), c(180, -90), c(180, 90), c(-180, 90), c(-180, -90)
))) |> st_sfc(crs = 4326)

# Subtract the region from the world to create a mask
mask <- st_difference(world, st_union(bham_boundary))

# Show the result
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addTiles(urlTemplate = "https://{s}.tiles.openrailwaymap.org/standard/{z}/{x}/{y}.png",
           attribution = "© OpenRailwayMap contributors") %>%
  setView(lng = -1.8904, lat = 52.4862, zoom = 10)%>%
  addPolygons(
    color = "#3388ff",
    weight = 1,
    fillOpacity = 0.3,
    highlightOptions = highlightOptions(
      color = "white",
      weight = 3,
      bringToFront = TRUE
    ),
    label = ~most_common_postcode,
    labelOptions = labelOptions(
      textsize = "13px",
      direction = "auto"
    )
  )



