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


leaflet_plot <- function(le_data, le_year) {

  # Extract selected year column
  le_column <- colnames(le_data)[grepl(paste0("x",le_year), colnames(le_data))]
  le_data <- le_data %>%
   # rename(life_expectancy = !!sym(le_column))
   # rename(life_expectancy = all_of(le_column))
     rename(!!"life_expectancy" := !!sym(le_column))
    
    
  # Subsets by sex
  data_male <- le_data %>% filter(sex == "Male")
  data_female <- le_data %>% filter(sex == "Female")
  data_both <- le_data %>% filter(sex == "Both")
  
  # Palettes
  pal_male <- colorNumeric("YlGnBu", domain = data_male$life_expectancy)
  pal_female <- colorNumeric("RdPu", domain = data_female$life_expectancy)
  pal_both <- colorNumeric("Greens", domain = data_both$life_expectancy)
  
  # Get 5 safe breaks and color scales
  get_legend_data <- function(data, palette) {
    rng <- range(data$life_expectancy, na.rm = TRUE)
    breaks <- seq(rng[1], rng[2], length.out = 5)
    breaks <- pmin(pmax(breaks, rng[1]), rng[2])  # clamp to domain
    colors <- palette(breaks)
    list(breaks = as.integer(breaks), colors = colors)
  }
  
  legend_info <- list(
    Male = get_legend_data(data_male, pal_male),
    Female = get_legend_data(data_female, pal_female),
    Both = get_legend_data(data_both, pal_both)
  )
  
  map <- leaflet() %>%
   addProviderTiles("CartoDB.Positron") %>%

    setView(lng = -1.5, lat = 54.5, zoom = 6) %>%
    
    addPolygons(data = data_male, group = "Male",
                fillColor = ~pal_male(life_expectancy), weight = 1, color = "white",
                dashArray = "3", fillOpacity = 0.7,
                highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
                label = ~paste0(LAD23NM, ": ", life_expectancy, " years")) %>%
    
    addPolygons(data = data_female, group = "Female",
                fillColor = ~pal_female(life_expectancy), weight = 1, color = "white",
                dashArray = "3", fillOpacity = 0.7,
                highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
                label = ~paste0(LAD23NM, ": ", life_expectancy, " years")) %>%
    
    addPolygons(data = data_both, group = "Both",
                fillColor = ~pal_both(life_expectancy), weight = 1, color = "white",
                dashArray = "3", fillOpacity = 0.7,
                highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
                label = ~paste0(LAD23NM, ": ", life_expectancy, " years")) %>%
    
    addLayersControl(
      baseGroups = c("Male", "Female", "Both"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # Embed legend info
  legend_json <- toJSON(legend_info, auto_unbox = TRUE)
  
  js_code <- paste0(
    "function(el, x) {
      var map = this;
      var legends = ", legend_json, ";

      function makeGradient(colors) {
        return 'linear-gradient(to right, ' + colors.join(',') + ')';
      }

      function updateLegend(group) {
        $('.legend').remove();
        var info = legends[group];
        if (!info) return;

        var legend = L.control({position: 'topright'});
        legend.onAdd = function() {
          var div = L.DomUtil.create('div', 'info legend');
          div.innerHTML += '<strong>Life Expectancy (' + group + ')</strong><br>';
          div.innerHTML += '<div style=\"width: 220px; height: 20px; background: ' + makeGradient(info.colors) + '; border: 1px solid #ccc; border-radius: 4px; margin: 6px auto;\"></div>';
          div.innerHTML += '<div style=\"display: flex; justify-content: space-between; width: 220px; margin: 0 auto; font-size: 12px; color: #555;\">' +
            info.breaks.map(val => '<span>' + val + '</span>').join('') +
            '</div>';
          return div;
        };
        legend.addTo(map);
      }

      updateLegend('Male');
      map.on('baselayerchange', function(e) {
        updateLegend(e.name);
      });
    }"
  )
  
  map <- onRender(map, js_code)
  return(map)
}






