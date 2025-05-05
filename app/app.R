library(shiny)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(leaflet)

source("map_le.R")

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
            h2("General Public")),
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
}
  

shinyApp(ui, server)
