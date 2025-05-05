######## train maps from jpeg
library(tidyverse)
library(ggplot2)
library(jpeg)


#######

plot_le_train_map<-function(){

img <- readJPEG("app/data/map.jpg")

le<-read.csv("app/data/birmingham_routes_maps_le.csv")
le_long <- pivot_longer(le, cols = c("Male", "Female"), names_to = "Gender", values_to = "Value")
le_long$y_offset <- ifelse(le_long$Gender == "Male", le_long$y - 0.275, le_long$y + 0.275)

ggplot(le_long, aes(x = x, y = y_offset, label = Value, color = Gender)) +
 background_image(img) +
  geom_text(size = 4, fontface = "bold") +
  scale_color_manual(values = c("Male" = "darkblue", "Female" = "darkred")) +
  theme(
    panel.grid.major = element_line(size = 2, linetype = 'solid',
                                    colour = "black"), 
    panel.grid.minor = element_line(size = 1, linetype = 'solid',
                                    colour = "black")
  )+
  xlim(-7.5,7.5)+
  ylim(-12.25, 12.25)+
  theme_void()+
  theme(legend.position="none")
}

