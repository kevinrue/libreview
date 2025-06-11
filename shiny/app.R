library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyverse)
library(hms)
library(ComplexHeatmap)
library(emoji)
library(RColorBrewer)
library(yaml)

source("constants.R")
source("import.R")
source("cleanup.R")
source("banners.R")
source("plot_timeline_recent.R")
source("heatmap_time_all.R")
source("cluster_time_all.R")
source("plot_time_overlaid.R")
source("plot_histogram_recent.R")
source("print_stats_all.R")
source("help.R")

glucose_data <- import_glucose_data(default_glucose_files)

date_annotations <- import_date_annotations(default_date_annotations_file)
date_annotations <- add_missing_date_annotations(glucose_data, date_annotations) %>% 
  mutate(
    type = refactor_na_last(type)
  )

date_type_colors <- import_date_type_colors(default_day_type_file)
custom_date_type_colors <- !is.null(date_type_colors)

config <- yaml::read_yaml("config.yaml")

ui <- page_navbar(
  id = page_navbar_id,
  title = "LibreView",
  navbar_options = navbar_options(
    bg = "#2D89C8",
    theme = "dark"
  ),
  fillable = FALSE,
  nav_panel(
    title = recent_data_id,
    card(
      p(emoji("lemon"), em("When life gives you data... make a dashboard!"), emoji("milk_glass"), align="center")
    ),
    card(height = "700px",
      card_header("Recent days"),
      layout_sidebar(
        sidebar = sidebar(
          open = "open", # "closed"
          numericInput("recent_days", "Number of days", 14L, min = 1, max = 30),
          checkboxInput("highlight_weekends", label = "Show weekends", value = TRUE)
        ),
        navset_card_pill(
          placement = "above",
          nav_panel(title = "Timeline",
            plotOutput("plot_timeline_recent", width = "100%", height = "400px",
              click = clickOpts(id ="plot_timeline_recent_click"))),
          nav_panel(title = "Heatmap",
            uiOutput("banner_heatmap_missing_date_annotations"),
            uiOutput("banner_heatmap_missing_custom_date_type_colours"),
            plotOutput("heatmap_time_all", width = "100%", height = "400px")
          ),
          nav_panel(title = "Histogram",
            plotOutput("plot_histogram_recent", width = "100%", height = "400px")
          )
        )
      )
    ),
    uiOutput("print_stats_all")
  ),
  nav_panel(
    title = overlay_days_id,
    layout_columns(
      col_widths = c(3, 9),
      card(
        uiOutput("date_annotation_file_ui")
      ),
      card(
        uiOutput("banner_overlaid_missing_date_annotations"),
        uiOutput("banner_overlaid_missing_custom_date_type_colours"),
        plotOutput("plot_time_overlaid", width = "100%", height = "400px")
      )
    )
  ),
  nav_spacer(),
  # nav_menu(
  #   title = "Links",
  #   align = "right",
  #   nav_item(tags$a("Posit", href = "https://posit.co")),
  #   nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  # ),
  nav_item(shiny::actionLink("help_main",label = "", icon = icon("question-circle")))
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    glucose_data = glucose_data,
    date_annotations = date_annotations,
    date_type_colors = date_type_colors
  )
  
  observeEvent(rv$glucose_data, {
    if (is.null(rv$glucose_data)) {
      showModal(
        modalDialog(
          p("No CSV file found under", tags$code("../data/glucose/"), ". Please select glucose files below."),
          shiny::fileInput(inputId = "glucose_files", label = "Glucose files", multiple = TRUE, accept = ".csv"),
          footer = NULL
        )
      )
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input[["help_main"]], {
    showModal(
      modalDialog(
        size = "xl",
        easyClose = TRUE,
        get_help_modal_ui(input[[page_navbar_id]])
      )
    )
  })
  
  observeEvent(input[["glucose_files"]], {
    if (!is.null(input[["glucose_files"]])) {
      glucose_data <- import_glucose_data(input[["glucose_files"]][["datapath"]])
      if (!is.null(glucose_data)) {
        rv$glucose_data <- glucose_data
        rv$date_annotations <- add_missing_date_annotations(glucose_data, date_annotations) %>% 
          mutate(
            type = refactor_na_last(type)
          )
        updatePickerInput(
          session = session, inputId = "day_type",
          choices = levels(rv$date_annotations$type),
          selected = levels(rv$date_annotations$type),
        )
        removeModal()
      }
    }
  })
  
  output$date_annotation_file_ui <- renderUI({
    if (all(rv$date_annotations$type == "NA")) {
      # fileInput("date_annotation_file", label = "Date annotations", multiple = FALSE, accept = ".csv")
      NULL
    } else {
      tagList(
        shinyWidgets::pickerInput(
          "day_type", "Type",
          choices = levels(rv$date_annotations$type),
          selected = levels(rv$date_annotations$type),
          options = pickerOptions(
            actionsBox = TRUE,
            selectedTextFormat = "count > 3"
          ),
          multiple = TRUE
        ),
        checkboxInput("plot_time_overlaid_color_logical", "Color by type", value = TRUE)
      )
    }
  })
  
  observeEvent(input[["date_annotation_file"]], {
    if (!is.null(input[["date_annotation_file"]])) {
      date_annotations <- import_date_annotations(input[["date_annotation_file"]][["datapath"]])
      if (!is.null(date_annotations)) {
        new_date_annotations <- add_missing_date_annotations(rv$glucose_data, date_annotations) %>% 
          mutate(
            type = refactor_na_last(type)
          )
        new_date_type_colors <- add_missing_date_type_colors(rv$date_type_colors, date_annotations)
        updatePickerInput(
          session = session, inputId = "day_type",
          choices = levels(new_date_annotations$type),
          selected = levels(new_date_annotations$type),
        )
        rv$date_annotations <- date_annotations
        rv$date_type_colors <- new_date_type_colors
        removeModal()
      }
    }
  })
  
  output$plot_timeline_recent <- renderPlot({plot_timeline_recent(
    rv$glucose_data,
    config,
    input[["recent_days"]],
    input[["highlight_weekends"]],
    rv[["click_datetime"]]
  )})
  
  observeEvent(input[["plot_timeline_recent_click"]], {
    click_datetime <- input[["plot_timeline_recent_click"]][["x"]]
    rv[["click_datetime"]] <- click_datetime
  })
  
  output$plot_time_overlaid <- renderPlot({plot_time_overlaid(
    rv$glucose_data$historic,
    rv$date_annotations,
    config,
    input[["day_type"]],
    input[["plot_time_overlaid_color_logical"]],
    rv$date_type_colors
  )})
  
  output$heatmap_time_all <- renderPlot({heatmap_time_all(
    rv$glucose_data$historic,
    rv$date_annotations,
    input[["recent_days"]],
    input[["highlight_weekends"]],
    cluster_days = FALSE,
    rv$date_type_colors
  )})
  
  output$plot_histogram_recent <- renderPlot({plot_histogram_recent(
    rv$glucose_data$historic,
    config,
    input[["recent_days"]]
  )})
  
  output$print_stats_all <- renderUI({print_stats_all(
    rv$glucose_data$historic
  )})
  
  output$banner_heatmap_missing_date_annotations <- renderUI({
    if (all(rv$date_annotations$type == "NA")) {
      card(
        style="color:#8a8a86;background-color:#f3f593;",
        height = "60px",
        p(
          emoji("light bulb"),
          em("Tip: Import date annotations to display them on the side of the heatmap!"),
          actionLink("date_annotation_modal_open", "Click here to import")
        )
      )
    }
  })
  
  output$banner_heatmap_missing_custom_date_type_colours <- renderUI({
    banner_missing_custom_date_type_colours(rv$date_annotations, custom_date_type_colors)
  })
  
  output$banner_overlaid_missing_date_annotations <- renderUI({
    if (all(rv$date_annotations$type == "NA")) {
      card(
        style="color:#8a8a86;background-color:#f3f593;",
        height = "60px",
        p(
          emoji("light bulb"),
          em("Tip: Import date annotations to color days by type!"),
          actionLink("date_annotation_modal_open", "Click here to import")
        )
      )
    }
  })
  
  output$banner_overlaid_missing_custom_date_type_colours <- renderUI({
    banner_missing_custom_date_type_colours(rv$date_annotations, custom_date_type_colors)
  })
  
  observeEvent(input[["date_annotation_modal_open"]], {
    showModal(
      modalDialog(
        size = "xl",
        easyClose = TRUE,
        fileInput("date_annotation_file", label = "Date annotations", multiple = FALSE, accept = ".csv")
      )
    )
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
