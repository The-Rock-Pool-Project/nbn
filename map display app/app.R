library(shiny)
library(shinydashboard)
library(tidyverse)
library(pool)

# Load species list
species_df <- read_csv("UK Marine NNS.csv") %>%
  mutate(id = str_replace_all(Taxon_name, " ", "_"))

# load records data
records_df <- read_csv("records_per_species_year.csv") %>%
  mutate(id = str_replace_all(species_name, " ", "_"))

source("DB log in dets.R")

# Set up database connection pool
pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = localdb,
  host = '35.177.196.110',
  username = localuser,
  password = localuserpassword
)



ui <- dashboardPage(
  dashboardHeader(title = "UK Marine NNS Maps"),
  dashboardSidebar(
    selectInput("species", "Select a species:",
                choices = setNames(species_df$id, paste0(species_df$Common_name, " (", species_df$Taxon_name, ")")),
                selected = species_df$id[1],
                selectize = TRUE),
    br()
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
        column(4,
               div(class = "species-img", uiOutput("species_image")),
               div(style = "font-size: 12px; margin-top: 6px; color: #555;", textOutput("species_attribution"))
        ),
        column(8,
               htmlOutput("species_label"),
               textOutput("species_comments"),
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
    ),
    
    #time plot
    fluidRow(
      box(
        width = 12, status = "info", solidHeader = TRUE,
        title = "UK Records Over Time",
        plotOutput("record_plot", height = "300px")
      )
    ),
    # Comments section
    fluidRow(
      box(
        title = "Comments", width = 12, status = "warning", solidHeader = TRUE,
        textInput("comment_name", "Your name:", "", width = "100%"),
        textAreaInput("comment_input", "Leave a comment:", "", rows = 3, width = "100%"),
        actionButton("submit_comment", "Submit", icon = icon("paper-plane")),
        br(), br(),
        dataTableOutput("comment_history")
      )
    ),
    
    
    #RPP logo
    fluidRow(
      column(12, align = "center",
             tags$img(src = "TRPP.png", height = "60px", style = "margin-top: 20px;"))
    )
    
    
  )
  
  
)

server <- function(input, output, session) {
  
  refresh_comments <- reactiveVal(0)
  
  addResourcePath("static_maps", "outputs/static")
  addResourcePath("animated_maps", "outputs/animated")
  addResourcePath("species_images", "images")
  
  image_info <- read_csv("species_image_info.csv") %>%
    mutate(id = str_replace_all(species, " ", "_"))
  
  get_attribution <- reactive({
    attr_row <- filter(image_info, id == input$species)
    if (nrow(attr_row) > 0 && !is.na(attr_row$attribution)) {
      return(attr_row$attribution)
    } else {
      return("Image source: unknown or not credited")
    }
  })
  
  
  
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
  
  output$species_image <- renderUI({
    file_base <- input$species
    image_file <- paste0(file_base, ".jpg")
    image_path <- file.path("species_images", image_file)
    
    if (!file.exists(file.path("images", image_file))) {
      image_path <- "species_images/placeholder.jpg"
    }
    
    tags$img(src = image_path, style = "width: 100%; height: 350px; object-fit: cover; border-radius: 6px; border: 1px solid #CCC;")
  })
  
  
  output$species_label <- renderUI({
    info_row <- filter(species_df, id == input$species)
    sci <- info_row$Taxon_name
    com <- info_row$Common_name
    mba <- info_row$MBA_guide == "Y"
    comments <- info_row$Comments
    
    record_count <- filter(records_df, id == input$species)$Total_records
    record_text <- if (!is.na(record_count)) {
      glue::glue("<div style='margin-top: 6px; font-size: 13px; color: #444;'>Total UK NBN records: {record_count}</div>")
    } else {
      ""
    }
    
    
    # MBA guide badge
    tags <- if (!is.na(mba)) {
      "<span class='tag'>MBA NNS Guide</span>"
    } else {
      ""
    }
    
    HTML(glue::glue(
      "<div>
     <span style='font-size: 20px; font-weight: bold;'>{com}</span><br/>
     <span style='font-style: italic; font-size: 16px; color: #555;'>{sci}</span><br/>
     <div style='margin-top: 6px;'>{tags}</div>
     {record_text}
   </div>"
    ))
    
  })
  
  
  output$species_attribution <- renderText({
    get_attribution()
  })
  
  output$species_comments <- renderText({
    info_row <- filter(species_df, id == input$species)

    if (!is.na(info_row$Comments)) {
      info_row$Comments
    } else {
      "No additional information available."
    }
  })
  
  output$record_plot <- renderPlot({
    this_species <- input$species
    species_row <- filter(records_df, id == this_species)
    
    # Extract year columns (e.g. 2003, 2004, ...)
    year_cols <- names(species_row)[grepl("^\\d{4}$", names(species_row))]
    
    # Pivot to long format and compute cumulative sum
    plot_data <- pivot_longer(species_row, cols = all_of(year_cols), names_to = "year", values_to = "records") %>%
      mutate(
        year = as.integer(year),
        records = replace_na(records, 0)
      ) %>%
      arrange(year) %>%
      mutate(cumulative = cumsum(records))
    
    ggplot(plot_data, aes(x = year, y = cumulative)) +
      geom_line(color = "#2176FF", size = 1.5) +
      geom_point(color = "#2176FF", size = 2) +
      labs(x = "Year", y = "Cumulative Records", title = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
      )
  })
  
  observeEvent(input$submit_comment, {
    req(input$species, input$comment_input, input$comment_name)
    
    species_id <- input$species
    comment_text <- trimws(input$comment_input)
    user_id <- trimws(input$comment_name)
    
    if (comment_text == "" || user_id == "") return(NULL)
    
    DBI::dbExecute(pool, "
    INSERT INTO species_comments (species_id, comment_text, user_id)
    VALUES (?, ?, ?)",
                   params = list(species_id, comment_text, user_id)
    )
    
    updateTextInput(session, "comment_name", value = "")
    updateTextAreaInput(session, "comment_input", value = "")
    
    # 🔁 Trigger comment history refresh
    refresh_comments(refresh_comments() + 1)
  })
  
  
  output$comment_history <- renderDataTable({
    refresh_comments()  # ❗ depend on trigger
    req(input$species)
    
    DBI::dbGetQuery(pool, "
    SELECT comment_text AS Comment, user_id AS Name, comment_time AS 'Time posted'
    FROM species_comments
    WHERE species_id = ?
    ORDER BY comment_time DESC",
                    params = list(input$species)
    )
  })
  
  
  
  
}

shinyApp(ui, server)
