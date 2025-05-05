######## train maps from jpeg grid for x, y coordinates
library(tidyverse)
library(ggplot2)
library(jpeg)
library(grid)


img <- readJPEG("app/data/map.jpg")
alpha <- array(0.3, dim = c(dim(img)[1], dim(img)[2]))  # 30% opacity
img_with_alpha <- array(NA, dim = c(dim(img)[1], dim(img)[2], 4))
# Copy RGB channels
img_with_alpha[,,1:3] <- img
#Add alpha channel
img_with_alpha[,,4] <- alpha
g <- rasterGrob(img_with_alpha, width = unit(1, "npc"), height = unit(1, "npc"))

le<-read.csv("app/data/birmingham_routes_maps_le.csv")
le_long <- pivot_longer(le, cols = c("Male", "Female"), names_to = "Gender", values_to = "Value")
le_long$y_offset <- ifelse(le_long$Gender == "Male", le_long$y - 0.275, le_long$y + 0.275)

ggplot(le_long, aes(x = x, y = y_offset, label = Value, color = Gender)) +
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  # Add image
  geom_text(size = 4, fontface = "bold") +  # Add your text labels
  scale_color_manual(values = c("Male" = "darkblue", "Female" = "darkred")) +
    theme(
         panel.grid.major = element_line(size = 1, linetype = 'solid', colour = "black"), 
         panel.grid.minor = element_line(size = 1, linetype = 'solid', colour = "grey"),
         panel.background = element_blank(),  # Transparent panel so image shows
         plot.background = element_blank()    # Transparent plot background
       ) +
  scale_x_continuous(
             limits = c(-7.5, 7.5),
             minor_breaks = seq(-7.5, 7.5, by = 0.5)) +
  scale_y_continuous(
             limits = c(-12.25, 12.25),
             minor_breaks = seq(-12.25, 12.25, by = 0.5))+
  theme(legend.position="none")













ggplot(le_long, aes(x = x, y = y_offset, label = Value, color = Gender)) +
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  # Add image
  geom_text(size = 4, fontface = "bold") +  # Add your text labels
  scale_color_manual(values = c("Male" = "darkblue", "Female" = "darkred")) +
  theme(
    panel.grid.major = element_line(size = 1, linetype = 'solid', colour = "black"), 
    panel.grid.minor = element_line(size = 1, linetype = 'solid', colour = "grey"),
    panel.background = element_blank(),  # Transparent panel so image shows
    plot.background = element_blank()    # Transparent plot background
  ) +
  scale_x_continuous(
    limits = c(-7.5, 7.5),
    minor_breaks = seq(-7.5, 7.5, by = 0.5)
  ) +
  scale_y_continuous(
    limits = c(-12.25, 12.25),
    minor_breaks = seq(-12.25, 12.25, by = 0.5)
  )