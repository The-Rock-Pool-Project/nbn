library(shiny)
library(tidyverse)
library(DT)

# Load data
data <- read.csv("outputs/all_region_recs.csv")

# Convert to long format for plotting
data_long <- data %>%
  pivot_longer(-Region, names_to = "Species", values_to = "Count")

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
                  selected = unique(data$Region)[1])
    ),
    mainPanel(
      fluidRow(
        column(6,
               h4("Species Abundance"),
               plotOutput("barplot", height = "600px")
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
      mutate(Category = categorise_species(Count))
  })
  
  output$barplot <- renderPlot({
    ggplot(region_data(), aes(x = reorder(Species, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(x = "Species", y = "Observation Count") +
      theme_minimal()
  })
  
  output$species_table <- renderDataTable({
    dat <- region_data() %>%
      select(Species, Count, Category) %>%
      arrange(desc(Count))
    
    datatable(dat, options = list(pageLength = 20), rownames = FALSE) %>%
      formatStyle(
        'Category',
        target = 'row',
        backgroundColor = styleEqual(
          c("Abundant", "Common", "Occasional", "Previously recorded", "Not recorded"),
          c("#1b7837", "#5aae61", "#a6dba0", "#f6e8c3", "#d73027")
        )
      )
  })
  
}

shinyApp(ui, server)
