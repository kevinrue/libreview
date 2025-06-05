library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyverse)
library(hms)
library(pheatmap)
library(emoji)
library(RColorBrewer)

source("constants.R")
source("import.R")
source("cleanup.R")
source("plot_time_all.R")
source("plot_time_overlaid.R")
source("plot_histogram_all.R")
source("print_stats_all.R")

glucose_data <- import_glucose_data()

date_annotations <- import_date_annotations()
date_annotations <- add_missing_date_annotations(glucose_data, date_annotations) %>% 
  mutate(
    type = refactor_na_last(type)
  )

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
    card(
      p(emoji("lemon"), em("When life gives you data... make a dashboard!"), emoji("milk_glass"), align="center")
    ),
    card(
      card_header("Timeline"),
      layout_sidebar(
        sidebar = sidebar(
          open = "closed",
          checkboxInput("highlight_weekends", label = "Highlight weekends", value = TRUE)
        ),
        plotOutput("plot_time_all", width = "100%", height = "400px")
      )
    ),
    plotOutput("plot_histogram_all", width = "100%", height = "400px"),
    uiOutput("print_stats_all"),
    p("TODO: restrict to last seven days.")
  ),
  nav_panel(
    title = "Overlay days",
    layout_columns(
      col_widths = c(3, 9),
      card(
        shinyWidgets::pickerInput(
          "day_type", "Type",
          choices = c(sort(unique(date_annotations$type))),
          selected = c(sort(unique(date_annotations$type))),
          options = pickerOptions(
            actionsBox = TRUE,
            selectedTextFormat = "count > 3"
          ),
          multiple = TRUE
        )
      ),
      card(
        plotOutput("plot_time_overlaid", width = "100%", height = "400px")
      )
    )
  ),
  nav_panel(
    title = "Global settings"
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
    glucose_data$historic, config, input[["highlight_weekends"]]
  )})
  
  output$plot_time_overlaid <- renderPlot({plot_time_overlaid(
    glucose_data$historic, date_annotations, input$day_type, config
  )})
  
  output$plot_histogram_all <- renderPlot({plot_histogram_all(glucose_data$historic, config)})
  
  output$print_stats_all <- renderUI({print_stats_all(glucose_data$historic)})
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
