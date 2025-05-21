library(shiny)
library(bslib)
library(ggplot2)

source("plot_time_all.R")

glucose_data <- read_csv("../data/KevinRue_glucose_21-5-2025.csv", skip = 1, show_col_types = FALSE)

ui <- page_navbar(
  title = "LibreView",
  navbar_options = navbar_options(
    bg = "#2D89C8",
    theme = "dark"
  ),
  nav_panel(
    title = "Full timeline",
    p("TODO: restrict to last seven days."),
    plotOutput("plot_time_all")
  ),
  nav_panel(title = "Two", p("Second page content.")),
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
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
