library(tidyverse)
library(gtfsio)
library(osmdata)
library(sf)
library(sfheaders)
library(stringr)

###### tram lines download data
#api keys
app_id <- "ecfd9ae9"
app_key <- "69d479ff971d9d14000cfb389054ed51"

#download url
gtfs_url <- paste0(
  "http://api.tfwm.org.uk/gtfs/tfwm_gtfs.zip?app_id=",
  app_id,
  "&app_key=",
  app_key
)

gtfs_path <- "app/data/tfwm_gtfs.zip"
download.file(gtfs_url, destfile = gtfs_path, mode = "wb")

gtfs_data <- import_gtfs("app/data/tfwm_gtfs.zip")

tram_routes <- gtfs_data$routes %>% filter(route_type == 0, route_short_name=="Metro")
tram_trips <- gtfs_data$trips %>% filter(route_id %in% tram_routes$route_id)
tram_stop_times <- gtfs_data$stop_times %>% filter(trip_id %in% tram_trips$trip_id)
tram_stops <- gtfs_data$stops %>% filter(stop_id %in% tram_stop_times$stop_id)

tram_stops<-tram_stops %>%
              mutate(stop_name = str_remove(stop_name, "\\s*\\([^\\)]+\\)"),
                 stop_name = str_remove(stop_name,"The|Central|Grand central|Outer Circle|Parkway!Great Western|Village|Booth Street|Station"))
          
tram_stops_sf <- st_as_sf(tram_stops, coords = c("stop_lon", "stop_lat"), crs =  4326)


st_write(tram_stops_sf, "app/datatram_stops_sf.gpkg")
st_write(tram_stops_sf, "app/datatram_stops_sf.shp")
##################################################################################

###### train lines data

birmingham_bbox <- getbb("Birmingham, UK")


birmingham_osm <- opq(birmingham_bbox) %>%
                                add_osm_feature(key = "railway", value = c("rail", "station")) %>%
                                osmdata_sf()

stations<- birmingham_osm$osm_points %>%
                              filter(railway =="station") %>%
                              filter(!is.na(name)) %>%
                              select(name, geometry)

routes <- list(
  "North" = c("New Street", "Duddeston", "Aston", "Gravelly Hill",
              "Erdington", "Chester Road", "Wide Green", "Sutton Coldfield", "Four Oaks"),
  
  "South" = c("New Street", "Five Ways", "University", "Selly Oak",
              "Bournville", "Kings Norton","Northfield", "Longbridge"),
  
  
  "West" = c("New Street", "Snow Hill", "St. Paul's", "Jewellery Quarter",  "Winson Green","Handsworth"),
  
  "East" = c("New Street", "Moor Street", "Bordesley", "Small Heath", "Tyseley", "Acocks Green")
)

selected_stations<-unique(unlist(routes, use.names=FALSE))


stations__to_plot<-rbind(stations, tram_stops_sf) %>%
                      filter(grepl(paste(selected_stations, collapse="|"), name)) %>%
                      mutate(name=str_extract_all(name, paste(selected_stations, collapse="|"))) %>%
                      distinct(name, .keep_all = TRUE) # some stations have slightly different coordinates up to three

##################################################

geo <- st_read("app/data/birmingham_adjusted_routes.geojson")


points <- geo %>% filter(type == "point")


points <- geo %>% filter(type == "point")

# Extract coordinates and save to CSV
points_df <- points %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

write.csv(points_df, "stations.csv", row.names = FALSE)


stations_to_plot <- readRDS("app/data/stations_to_plot.rds")

write.csv(stations_to_plot, "app/data/stations_to_plot.csv")



