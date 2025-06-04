library(shiny)
library(shinyWidgets)
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

glucose_data_all <- do.call(
  "rbind",
  lapply(
    list.files("../data/glucose/", full.names = TRUE),
    read_csv,
    skip = 1,
    col_types = glucose_file_spec
  )
) %>% 
  distinct()

glucose_data_historic <- glucose_data_all %>% 
  filter(
    `Record Type` %in% c(0)
  ) %>% 
  select(Device, `Serial Number`, `Device Timestamp`, `Historic Glucose mmol/L`)

glucose_data_scan <- glucose_data_all %>% 
  filter(
    `Record Type` %in% c(1)
  ) %>% 
  select(Device, `Serial Number`, `Device Timestamp`, `Scan Glucose mmol/L`)

glucose_data_notes <- glucose_data_all %>% 
  filter(
    `Record Type` %in% c(6),
    !is.na(Notes)
  ) %>% 
  select(Device, `Serial Number`, `Device Timestamp`, `Notes`)

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
        shinyWidgets::pickerInput(
          "day_type", "Type",
          choices = c(sort(unique(date_annotations$type))),
          selected = character(0),
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
    glucose_data_historic, config, input[["highlight_weekends"]]
  )})
  
  output$plot_time_overlaid <- renderPlot({plot_time_overlaid(
    glucose_data_historic, date_annotations, input$day_type, config
  )})
  
  output$plot_histogram_all <- renderPlot({plot_histogram_all(glucose_data_historic, config)})
  
  output$print_stats_all <- renderUI({print_stats_all(glucose_data_historic)})
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
