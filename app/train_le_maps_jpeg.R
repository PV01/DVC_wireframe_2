######## train maps from jpeg
library(tidyverse)
library(ggplot2)
library(jpeg)
library(ggpubr)


## le at birth 2014-16
bham_female_le<-81.9
uk_female_le<-83.1
bham_male_le<-77.2
uk_male_le<-79.5


le_train_map<-function(){

img <- readJPEG("data/map.jpg")

le<-read.csv("data/birmingham_routes_maps_le.csv")
le_long <- pivot_longer(le, cols = c("Male", "Female"), names_to = "Gender", values_to = "Value")
le_long$y_offset <- ifelse(le_long$Gender == "Male", le_long$y - 0.275, le_long$y + 0.275)

ggplot(le_long, aes(x = x, y = y_offset, label = Value, color = Gender)) +
 background_image(img) +
  geom_text(size = 6, fontface = "bold") +
  scale_color_manual(values = c("Male" = "darkblue", "Female" = "darkred")) +
  xlim(-7.5,7.5)+
  ylim(-12.25, 12.25)+
  theme_void()+
  theme(legend.position="none")
}
##########################################
myMap <- leaflet(options = leafletOptions(minZoom = 11)) %>%
  addProviderTiles("OpenStreetMap") %>%
  setView( lng = -87.567215
           , lat = 41.822582
           , zoom = 11 ) %>%
  setMaxBounds( lng1 = -87.94011
                , lat1 = 41.64454
                , lng2 = -87.52414
                , lat2 = 42.02304 )

# display leaflet map
myMap
####################################
#######################################