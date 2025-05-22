library(shiny)
library(bslib)
library(ggplot2)

source("plot_time_all.R")
source("plot_time_overlaid.R")

glucose_data <- read_csv("../data/KevinRue_glucose_21-5-2025.csv", skip = 1, show_col_types = FALSE)
date_annotations <- read_csv("../data/date_annotations.csv", col_names = c("date", "type"), show_col_types = FALSE)

ui <- page_navbar(
  title = "LibreView",
  navbar_options = navbar_options(
    bg = "#2D89C8",
    theme = "dark"
  ),
  nav_panel(
    title = "Full timeline",
    em("When life gives you data..."),
    plotOutput("plot_time_all"),
    p("TODO: restrict to last seven days.")
  ),
  nav_panel(
    title = "Overlay days",
    layout_columns(
      col_widths = c(3, 9),
      card(
        selectInput("day_type", "Type", sort(unique(date_annotations$type)))
      ),
      card(
        plotOutput("plot_time_overlaid")
      )
    )
  ),
  nav_panel(title = "Three", p("Third page content.")),
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
  
  output$plot_time_all <- renderPlot({plot_time_all(glucose_data)})
  output$plot_time_overlaid <- renderPlot({plot_time_overlaid(glucose_data, date_annotations, input$day_type)})
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
