library(dplyr)
library(tidyr)
library(stringr)
library(ggimage)

# Read files
region_counts <- read.csv("all_region_recs.csv", check.names = F)
species_info <- read.csv("species_image_info_with_common_names.csv", encoding = "latin1")

# Clean names for matching
species_info <- species_info %>%
  mutate(
    species = str_trim(species),
    display_name = ifelse(
      is.na(common_name) | common_name == "",
      species,
      common_name
    )
  )

# Create name map
name_map <- species_info %>%
  select(species, display_name)

# Convert to long and clean
data_long <- region_counts %>%
  pivot_longer(-Region, names_to = "species", values_to = "Count") %>%
  mutate(species = str_trim(species)) %>%
  left_join(name_map, by = "species") %>%
  mutate(display_name = ifelse(is.na(display_name), species, display_name))


# Categorise species by count
categorise_species <- function(count) {
  case_when(
    count > 500 ~ "Abundant",
    count > 100 ~ "Common",
    count > 10  ~ "Occasional",
    count > 0   ~ "Previously recorded",
    TRUE        ~ "Not recorded"
  )
}

ui <- fluidPage(
  titlePanel("Marine Non-native Species by UK Region"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Choose a region:",
                  choices = unique(data$Region),
                  selected = unique(data$Region)[1]),
      numericInput("top_n", "Number of top species to show:", value = 5, min = 1, max = 50)
    ),
    mainPanel(
      fluidRow(
        column(6,
               h4("Species Abundance"),
               plotOutput("barplot", height = "600px"),
               downloadButton("download_plot", "Download Plot")
        ),
        column(6,
               h4("Species Category Table"),
               dataTableOutput("species_table")
        )
      )
    )
    
  )
)

server <- function(input, output) {
  
  region_data <- reactive({
    data_long %>%
      filter(Region == input$region) %>%
      mutate(Category = categorise_species(Count)) %>%
      arrange(desc(Count)) %>%
      slice_head(n = input$top_n) %>%
      mutate(image_file = paste0("www/thumbnails_rounded/", species, "_rounded.png"))
  })
  
  
  
  library(ggimage)
  
  output$barplot <- renderPlot({
    ggplot(region_data(), aes(x = reorder(display_name, Count), y = Count)) +
      geom_col(fill = "#2176FF") +
      geom_image(aes(image = image_file, y = Count + max(region_data()$Count) * (0.08 * (11/input$top_n))), 
                 size = 0.05 * (11/input$top_n), by = "width", asp = 1.5) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = scales::comma, n.breaks = 5) +
      labs(x = "Species", y = "Observations") +
      theme_minimal(base_family = "Chivo") +
      theme(
        plot.title = element_text(family = "Chivo", face = "bold", size = 18),
        axis.title.x = element_text(family = "Montserrat", size = 16),
        axis.text.x = element_text(family = "Montserrat", size = 14),
        axis.title.y = element_text(family = "Montserrat", size = 16),
        axis.text.y = element_text(family = "Montserrat", size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 40)
      )
  })
  
  
  
  
  
  
  
  output$species_table <- renderDataTable({
    dat <- region_data() %>%
      select(display_name, Count, Category) %>%
      arrange(desc(Count))
    
    datatable(dat, options = list(pageLength = 20), rownames = FALSE) %>%
      formatStyle(
        'Category',
        target = 'row',
        backgroundColor = styleEqual(
          c("Abundant", "Common", "Occasional", "Previously recorded", "Not recorded"),
          c("#2176FF", "#4D56F5", "#00A6FB", "#FDBD19", "#FF9895")  # primary + accents
        ),
        color = '#191D2D'  # text in Midnight Tide for contrast
      )
    
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("Invasive_species_records-Top_", input$top_n, "_species_",gsub(" ", "_", input$region), ".png")
    },
    content = function(file) {
      region_df <- region_data()
      
      # Load magick
      library(magick)
      library(ggimage)
      
      # Create plot with images (same as in renderPlot)
      p <- ggplot(region_df, aes(x = reorder(display_name, Count), y = Count)) +
        geom_col(fill = "#2176FF") +
        geom_image(aes(image = image_file, y = Count + max(region_data()$Count) *  (0.08 * (11/input$top_n))), 
                   size = 0.05 * (11/input$top_n), by = "width", asp = 1.5) +
        coord_flip(clip = "off") +
        scale_y_continuous(labels = scales::comma, n.breaks = 5) +
        labs(x = "", y = "Observations") +
        theme_minimal(base_family = "Chivo") +
        theme(
          plot.title = element_text(family = "Chivo", face = "bold", size = 18),
          axis.title.x = element_text(family = "Montserrat", size = 14),
          axis.text.x = element_text(family = "Montserrat", size = 12),
          axis.title.y = element_text(family = "Montserrat", size = 10),
          axis.text.y = element_text(family = "Montserrat", size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(t = 10, r = 20, b = 10, l = 0)
        )
      
      # Render to magick graphic device, then write to file
      img <- image_graph(width = 1000, height = 800, res = 244)
      print(p)
      dev.off()
      image_write(img, path = file, format = "png")
    }
  )
  
  
}

shinyApp(ui, server)
  