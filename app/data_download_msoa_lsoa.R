#### download msoa and lsoa

# Birmingham City Observatory: 
## https://cityobservatory.birmingham.gov.uk/explore/dataset/boundaries-msoa-wmca/information/?utm_source=chatgpt.com&disjunctive.local_authority_name&disjunctive.local_authority_code&disjunctive.msoa21nm&disjunctive.msoa21cd

# ArcGIS
# https://www.arcgis.com/home/item.html?id=d1363807d5594110ad0bcb37f11cb02e&sublayer=10&utm_source=chatgpt.com

##########################################

library(dplyr)
library(sf)
library(leaflet)

wm_msoa<-read_sf("app/data/boundaries-msoa-wmca.geojson")
bham_msoa<-read_sf("app/data/boundaries-msoa-wmca_selected.geojson")
  
wm_lsoa<-read_sf("app/data/boundaries-lsoa-wmca.geojson")
bham_lsoa<-read_sf("app/data/boundaries-lsoa-wmca_selected.geojson")


###################

leaflet(bham_lsoa) %>%
  addProviderTiles("CartoDB.Positron") %>% # nice light background
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
  )


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





