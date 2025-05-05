library(shiny)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(leaflet)

source("map_le.R")
source("train_le_maps_jpeg.R")

header <- dashboardHeader(disable=TRUE)

sidebar<-dashboardSidebar(
  width = 250,
  tags$div(
    id="sidebarID",
    class = "custom-sidebar-card",
    sidebarMenu(
      menuItem("Live Longer", tabName = "live_longer", icon = icon("chalkboard-teacher"), selected = TRUE),
      menuItem("Healthy Lifestyle", tabName = "healthy", icon = icon("heartbeat")),
      menuItem("Location", tabName = "location", icon = icon("globe")),
      menuItem("General Public", tabName = "public", icon = icon("users")),
      menuItem("General Public2", tabName = "public2", icon = icon("users")),
      menuItem("Council", tabName = "council", icon = icon("user-tie")),
      menuItem("About", tabName = "about", icon = icon("clipboard-list"))
    )
  )
)

body<-dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$style(".body{height:900px;}
                .footer{height:20px;text-align: center;}")
  ),
  tabItems(
    tabItem(tabName = "live_longer",
            h1("Want to live longer?"),
            
            h3("Surprising facts you may not know about.")
    ),
    tabItem(tabName = "healthy",
            fluidRow(
              h2("Adopt a healthy lifestyle"),
              column(width = 6,
                     img(src = "healthy_lifestyle.jpg", width = "100%", class = "lowered-img", alt = "Healthy Lifestyle Image"),
              )
            )
    ),
    tabItem(tabName = "location",
            h2("Location, location, location ..."),
            tabBox(
              id = "year_tabs",
              width = 12,
              tabPanel("2001 to 2003", leafletOutput("map_2001", height = "700px")),
              tabPanel("2016 to 2018", leafletOutput("map_2016", height = "700px")),
              tabPanel("2017 to 2019", leafletOutput("map_2017", height = "700px")),
              tabPanel("2018 to 2020", leafletOutput("map_2018", height = "700px")),
              tabPanel("2019 to 2021", leafletOutput("map_2019", height = "700px")),
              tabPanel("2020 to 2022", leafletOutput("map_2020", height = "700px")),
              tabPanel("2021 to 2023", leafletOutput("map_2021", height = "700px")),
              tabPanel("Change", leafletOutput("map_change", height = "700px"))
            )
          ),
    tabItem(tabName = "public",
            h2("General Public"),
            fluidRow(
              column(
                width = 9,  # Main area for the plot
                plotOutput("train_map", height = "700px")
              ),
              column(
                width = 3,  # Right-side column for the cards
                div(
                  style = "margin-bottom: 20px;",  # Space between the cards
                  box(
                    title = "Card 1",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    "Content for the first card."
                  )
                ),
                box(
                  title = "Card 2",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  "Content for the second card."
                )
              )
            )
    ),
    tabItem(tabName = "public2",
            h2("General Public"),
            
            # Custom styles
            tags$head(
              tags$style(HTML("
      .info-card {
        border-radius: 15px;
        border: 2px solid #ccc;
        padding: 15px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
        background-color: white;
        height: 120px;
        width: 95%; /* Slightly wider */
        display: flex;
        align-items: center;
        gap: 15px;
      }
      .icon-female {
        color: #e91e63;
        font-size: 50px;
      }
      .icon-male {
        color: #2196f3;
        font-size: 50px;
      }
      .text-box {
        font-size: 16px;
      }
      .female-value {
        font-weight: bold;
        color: #e91e63;
      }
      .male-value {
        font-weight: bold;
        color: #2196f3;
      }
    "))
            ),
            
            div(
              style = "position: relative;",
              plotOutput("train_map", height = "700px"),
              
              absolutePanel(
                top = "550px", right = "150px", width = "500px",  # Slightly wider container
                fixed = FALSE, draggable = FALSE,
                style = "display: flex; gap: 10px;",
                
                # Female card
                div(
                  style = "flex: 1;",
                  div(
                    class = "info-card",
                    icon("female", class = "icon-female"),
                    div(
                      class = "text-box",
                      htmlOutput("female_text")
                    )
                  )
                ),
                
                # Male card
                div(
                  style = "flex: 1;",
                  div(
                    class = "info-card",
                    icon("male", class = "icon-male"),
                    div(
                      class = "text-box",
                      htmlOutput("male_text")
                    )
                  )
                )
              )
            )
    ),
    tabItem(tabName = "council",
            h2("Council")),
    tabItem(tabName = "about",
            h2("About"))
  )
)



ui <- fluidPage(
  fluidRow(class="body",
    column(width = 1,
           sidebar),
    column(width = 8,
           body),
  ),
  fluidRow(class="footer",
           p("Pedro Veiga 2005", class = "custom-footer")
  )
)




server <- function(input, output, session) {
  output$map_2001 <- renderLeaflet({
    leaflet_plot(uk_le_data,2001)
  })
  
  output$map_2016 <- renderLeaflet({
    leaflet_plot(uk_le_data,2016)
  })
  
  
  output$map_2017 <- renderLeaflet({
    leaflet_plot(uk_le_data,2017)
  })
  
  output$map_2018 <- renderLeaflet({
    leaflet_plot(uk_le_data,2018)
  })
  
  output$map_2019 <- renderLeaflet({
    leaflet_plot(uk_le_data,2019)
  })
  
  output$map_2020 <- renderLeaflet({
    leaflet_plot(uk_le_data, 2020)
  })
  
  output$map_2021 <- renderLeaflet({
    leaflet_plot(uk_le_data,2021)
  })
  
  output$map_change <- renderLeaflet({
    leaflet_plot(uk_le_data,2023)
  })
  
  output$train_map<-renderPlot({
    le_train_map()
  })



female_life_expectancy <- 83.1  # Replace with your real value
male_life_expectancy <- 79.4    # Replace with your real value

output$female_text <- renderUI({
  female_life_expectancy <- 83.1  # Replace as needed
  HTML(paste("The average life expectancy of females in the UK is",
             sprintf("<span class='female-value'>%.1f</span> years.", female_life_expectancy)))
})

output$male_text <- renderUI({
  male_life_expectancy <- 79.4  # Replace as needed
  HTML(paste("The average life expectancy of males in the UK is",
             sprintf("<span class='male-value'>%.1f</span> years.", male_life_expectancy)))
})
} 

shinyApp(ui, server)
