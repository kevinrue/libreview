library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyverse)
library(hms)
library(ComplexHeatmap)
library(emoji)
library(RColorBrewer)
library(yaml)
library(pcaMethods)
library(umap)

source("constants.R")
source("actions.R")
source("alerts.R")
source("import.R")
source("cleanup.R")
source("banners.R")
source("datetime.R")
source("plot_timeline_recent.R")
source("heatmap_time_recent.R")
source("cluster_time_all.R")
source("plot_timeline_overlaid.R")
source("plot_histogram_recent.R")
source("plot_pca.R")
source("plot_umap.R")
source("print_stats_all.R")
source("help.R")

app_data <- import_app_data()

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
            uiOutput("banner_heatmap_time_recent"),
            plotOutput("heatmap_time_recent", width = "100%", height = "400px")
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
      card(
        card_header("Overlaid days"),
        layout_sidebar(
          sidebar = sidebar(
            open = "open", # "closed"
            uiOutput("date_annotation_file_ui")
          ),
          uiOutput("banner_plot_timeline_overlaid"),
          plotOutput("plot_timeline_overlaid", width = "100%", height = "400px")
        )
      )
    )
  ),
  nav_panel(
    title = dimred_id,
    layout_columns(
      card(
        card_header("Dimensionality reduction"),
        layout_sidebar(
          sidebar = sidebar(
            open = "open", # "closed"
            numericInput("dimred_point_size", "Point size", 5L, min = 1, max = 10),
            # uiOutput("date_annotation_file_ui")
          ),
          navset_card_pill(
            placement = "above",
            nav_panel(title = "PCA",
              plotOutput("plot_pca", width = "100%", height = "600px")
            ),
            nav_panel(title = "UMAP",
              plotOutput("plot_umap", width = "100%", height = "600px")
            )
          )
        )
      )
    )
  ),
  nav_spacer(),
  nav_item(shiny::actionLink("actions_icon", label = uiOutput("actions_icon"))),
  nav_item(shiny::actionLink("help_main", label = "", icon = icon("question-circle")))
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    glucose_data = app_data$glucose_data,
    date_annotations = app_data$date_annotations,
    date_type_colors = app_data$date_type_colors,
    custom_date_type_colors = !is.null(app_data$date_type_colors),
    alerts = count_alerts(
      app_data$date_annotations,
      !is.null(app_data$date_type_colors)
    )
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
    isolate({day_types <- levels(rv$date_annotations$type)})
    out <- tagList(
      textInput("search_notes", label = "Search notes", value = "", placeholder = "Keyword")
    )
    if (!all(rv$date_annotations$type == "NA")) {
      out <- append(out, tagList(
        shinyWidgets::pickerInput(
          "day_type", "Type",
          choices = day_types,
          selected = day_types,
          options = pickerOptions(
            actionsBox = TRUE,
            selectedTextFormat = "count > 3"
          ),
          multiple = TRUE
        ),
        checkboxInput("plot_timeline_overlaid_color_logical", "Color by type", value = TRUE)
      )
      )
    }
    return(out)
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
        rv$date_annotations <- new_date_annotations
        rv$date_type_colors <- new_date_type_colors
        rv$alerts <- count_alerts(new_date_annotations, rv$custom_date_type_colors)
        removeModal()
      }
    }
  })
  
  output$plot_timeline_recent <- renderPlot({plot_timeline_recent(
    rv$glucose_data,
    app_data$config,
    input[["recent_days"]],
    input[["highlight_weekends"]],
    rv[["click_datetime"]]
  )})
  
  observeEvent(input[["plot_timeline_recent_click"]], {
    click_datetime <- input[["plot_timeline_recent_click"]][["x"]]
    rv[["click_datetime"]] <- click_datetime
  })
  
  output$plot_timeline_overlaid <- renderPlot({plot_timeline_overlaid(
    rv$glucose_data,
    rv$date_annotations,
    app_data$config,
    input[["day_type"]],
    input[["plot_timeline_overlaid_color_logical"]],
    rv$date_type_colors,
    input[["search_notes"]]
  )})
  
  output$heatmap_time_recent <- renderPlot({heatmap_time_recent(
    rv$glucose_data$historic,
    rv$date_annotations,
    input[["recent_days"]],
    input[["highlight_weekends"]],
    cluster_days = FALSE,
    rv$date_type_colors
  )})
  
  output$plot_histogram_recent <- renderPlot({plot_histogram_recent(
    rv$glucose_data$historic,
    app_data$config,
    input[["recent_days"]]
  )})
  
  observe({
    rv$pca <- compute_pca(rv$glucose_data)
  })
  
  output$plot_pca <- renderPlot({plot_pca(
    rv$pca, rv$date_annotations,
    input[["dimred_point_size"]]
  )})
  
  output$plot_umap <- renderPlot({plot_umap(
    rv$pca, rv$date_annotations
  )})
  
  output$print_stats_all <- renderUI({print_stats_all(
    rv$glucose_data$historic
  )})
  
  output$banner_heatmap_time_recent <- renderUI({
    banner_heatmap_time_recent(rv$date_annotations, rv$custom_date_type_colors)
  })
  
  output$banner_plot_timeline_overlaid <- renderUI({
    banner_plot_timeline_overlaid(rv$date_annotations, rv$custom_date_type_colors)
  })
  
  observeEvent(input[["actions_icon"]], {
    showModal(
      modalDialog(
        size = "xl",
        easyClose = TRUE,
        tagList(
          h3("Actions available"),
          get_actions_modal_ui(rv$date_annotations, rv$custom_date_type_colors)
        )
      )
    )
  })
  
  output$actions_icon <- renderUI({
    tagList(emoji("bell"), " ", tags$sup("(", rv$alerts, ")"))
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
