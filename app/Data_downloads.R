library(tidyverse)
library(sf)
library(readxl)
library(janitor)
library(stringr)

# Download the geojson file
uk_geojson_url <- "https://opendata.arcgis.com/datasets/79a4e87783be4b6bbb96ddad6dda52a3_0.geojson"
uk_boundaries <- st_read(uk_geojson_url)

st_write(uk_boundaries, "app/data/uk_boundaries.geojson")

uk_boundaries<-st_read( "app/data/uk_boundaries.geojson")

# Life expectancy for local areas of Great Britain: between 2001 to 2003 and 2021 to 2023 ONS
#https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/lifeexpectancyforlocalareasofgreatbritain
life_expectancy<- read_excel("app/data/lifeexpectancylocalareas.xlsx", sheet=4, skip=5, .name_repair =  janitor::make_clean_names) %>%
                                   filter(age_band == 1) %>%
                                   select(1:6,10) 

# average life expectancy for male and females
both_sex_avg <- life_expectancy %>%
  group_by(period, country, area_type, area_code, area_name) %>%
  summarise(
    sex = "Both",
    life_expectancy = mean(life_expectancy, na.rm = TRUE),
    .groups = "drop"
  )


life_expectancy<- life_expectancy %>%
  bind_rows(both_sex_avg) %>%
  arrange(period, area_name, factor(sex, levels = c("Both","Female", "Male")))%>%
                                   pivot_wider(names_from = period, values_from = life_expectancy) %>%
                                   rename_with(~ str_replace_all(., " to ", "_")) %>%
                                   rename_with(~ if_else(str_detect(., "^\\d{4}_\\d{4}$"), paste0("x", .), .))%>%
                                   mutate(x2023=x2021_2023-x2001_2003)

write.csv(life_expectancy, "app/data/LifeExpectancyAtBirth.csv")

#######################################################################################################
uk_boundaries<-st_read( "app/data/uk_boundaries.geojson")
life_expectancy<-read.csv("app/data/LifeExpectancyAtBirth.csv")

uk_le_data <- uk_boundaries %>%
  left_join(life_expectancy, by = c("LAD23CD"="area_code", "LAD23NM"="area_name")) %>%
  filter(!(is.na(x2001_2003)))


st_write(uk_le_data, "app/data/uk_le_data.geojson")