library(shiny)
library(shinydashboard)
library(tidyverse)

# Load species list
species_df <- read_csv("UK Marine NNS.csv") %>%
  mutate(id = str_replace_all(Taxon_name, " ", "_"))

ui <- dashboardPage(
  dashboardHeader(title = "UK Marine NNS Maps"),
  dashboardSidebar(
    selectInput("species", "Select a species:",
                choices = setNames(species_df$id, species_df$Taxon_name),
                selected = species_df$id[1]),
    br(),
    downloadButton("download_static", "Download Static Map"),
    downloadButton("download_animated", "Download Animated Map")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
    .content-wrapper { background-color: #FFFFFF; }
    .species-img img {
      width: 100%;
      height: 350px;
      object-fit: cover;
      border-radius: 6px;
      border: 1px solid #CCC;
    }
    .map-img img {
      width: 100%;
      height: 500px;
      object-fit: contain;
      border-radius: 4px;
      border: 1px solid #DDD;
    }
    .box-header h3 {
      font-family: 'Montserrat', sans-serif;
      font-weight: bold;
    }
  "))),
    
    # Species image and info box
    fluidRow(
      box(
        width = 12, status = "success", solidHeader = TRUE,
        title = "Species Overview",
        column(4, div(class = "species-img", imageOutput("species_image", height = "auto"))),
        column(8,
               h3(textOutput("species_name")),
               p("This is placeholder text for additional species info (e.g. origin, family, description)."),
               radioButtons("map_type", "Select map type:", choices = c("Static map" = "static", "Animated timeline" = "animated"), inline = TRUE),
               br(),
               downloadButton("download_static", "Download Static Map"),
               downloadButton("download_animated", "Download Animated Map")
        )
      )
    ),
    
    # Map display area
    fluidRow(
      box(
        width = 12, status = "primary", solidHeader = TRUE,
        title = "Map View",
        div(class = "map-img", uiOutput("map_output"))
      )
    )
  )
  
  
)

server <- function(input, output, session) {
  
  
  addResourcePath("static_maps", "outputs/static")
  addResourcePath("animated_maps", "outputs/animated")
  
  
  get_static_path <- reactive({
    file.path("outputs/static", paste0(input$species, ".png"))
  })
  
  get_animated_path <- reactive({
    file.path("outputs/animated", paste0(input$species, ".gif"))
  })
  
  # Dynamic map display
  output$map_output <- renderUI({
    req(input$species)
    file_base <- input$species
    
    if (input$map_type == "static") {
      static_url <- file.path("static_maps", paste0(file_base, ".png"))
      tags$img(src = static_url, width = "100%",
               style = "border: 1px solid #ccc; border-radius: 6px;")
    } else {
      mp4_url <- file.path("animated_maps", paste0(file_base, ".mp4"))
      tags$video(
        src = mp4_url,
        type = "video/mp4",
        width = "100%",
        height = "500px",
        controls = NA,
        autoplay = NA,
        loop = NA,
        style = "border: 1px solid #ccc; border-radius: 6px;"
      )
    }
  })
  
  
  output$download_static <- downloadHandler(
    filename = function() paste0(input$species, "_static_map.png"),
    content = function(file) file.copy(get_static_path(), file)
  )
  
  output$download_animated <- downloadHandler(
    filename = function() paste0(input$species, "_animated_map.gif"),
    content = function(file) file.copy(get_animated_path(), file)
  )
  
  get_species_image_path <- reactive({
    file.path("images", paste0(input$species, ".jpg"))
  })
  
  output$species_image <- renderImage({
    path <- file.path("images", paste0(input$species, ".jpg"))
    if (file.exists(path)) {
      list(src = path, contentType = 'image/jpeg', width = "100%")
    } else {
      list(src = "images/placeholder.jpg", contentType = 'image/jpeg', width = "100%")
    }
  }, deleteFile = FALSE)
  
  output$species_name <- renderText({
    name <- filter(species_df, id == input$species)$Taxon_name
    name
  })
  
  
}

shinyApp(ui, server)
