library(shiny)
library(bslib)
library(tidyverse)
library(hms)
library(pheatmap)
library(emoji)
library(RColorBrewer)

source("constants.R")
source("plot_time_all.R")
source("plot_time_overlaid.R")
source("plot_histogram_all.R")
source("print_stats_all.R")

glucose_data <- read_csv("../data/glucose_data.csv", skip = 1, show_col_types = FALSE)

date_annotations_file <- "../data/date_annotations.csv"
if (file.exists(date_annotations_file)) {
  date_annotations <- read_csv(
    file = date_annotations_file,
    col_names = c("date", "type"),
    show_col_types = FALSE
  )
} else {
  date_annotations <- NULL
}
config <- yaml::read_yaml("config.yaml")

ui <- page_navbar(
  title = "LibreView",
  navbar_options = navbar_options(
    bg = "#2D89C8",
    theme = "dark"
  ),
  fillable = FALSE,
  nav_panel(
    title = "Full timeline",
    em("When life gives you data... make a dashboard!"),
    plotOutput("plot_time_all", width = "100%", height = "400px"),
    plotOutput("plot_histogram_all", width = "100%", height = "400px"),
    uiOutput("print_stats_all"),
    p("TODO: restrict to last seven days.")
  ),
  nav_panel(
    title = "Overlay days",
    layout_columns(
      col_widths = c(3, 9),
      card(
        selectInput(
          "day_type", "Type",
          choices = c(.all_label, sort(unique(date_annotations$type))),
          selected = .all_label,
          multiple = TRUE
        )
      ),
      card(
        plotOutput("plot_time_overlaid", width = "100%", height = "400px")
      )
    )
  ),
  nav_panel(
    title = "Global settings",
    checkboxInput("highlight_weekends", label = "Highlight weekends", value = TRUE)
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Posit", href = "https://posit.co")),
    nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$plot_time_all <- renderPlot({plot_time_all(
    glucose_data, config, input[["highlight_weekends"]]
  )})
  
  output$plot_time_overlaid <- renderPlot({plot_time_overlaid(
    glucose_data, date_annotations, input$day_type, config
  )})
  
  output$plot_histogram_all <- renderPlot({plot_histogram_all(glucose_data, config)})
  
  output$print_stats_all <- renderUI({print_stats_all(glucose_data)})
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
