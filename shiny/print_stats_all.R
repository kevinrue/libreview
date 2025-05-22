print_stats_all <- function(glucose_data) {
  plot_data <- glucose_data %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    separate(`Device Timestamp`, c("Date", "Time"), sep = " ") %>% 
    mutate(
      `Device Timestamp` = as_datetime(ymd_hm(paste(Sys.Date(), Time)))
    ) %>% 
    filter(!is.na(`Historic Glucose mmol/L`))
  
  day_start <- ymd_hm(paste(Sys.Date(), "06:00"))
  day_end <- ymd_hm(paste(Sys.Date(), "22:00"))
  
  min_value <- plot_data %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    min()
  median_value <- plot_data %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    median()
  mean_value <- plot_data %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    mean()
  q95_value <- plot_data %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    quantile(0.95)
  max_value <- plot_data %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    max()
  
  min_value_day <- plot_data %>% 
    filter(`Device Timestamp` > day_start & `Device Timestamp` < day_end) %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    min()
  median_value_day <- plot_data %>% 
    filter(`Device Timestamp` > day_start & `Device Timestamp` < day_end) %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    median()
  mean_value_day <- plot_data %>% 
    filter(`Device Timestamp` > day_start & `Device Timestamp` < day_end) %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    mean()
  q95_value_day <- plot_data %>% 
    filter(`Device Timestamp` > day_start & `Device Timestamp` < day_end) %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    quantile(0.95)
  max_value_day <- plot_data %>% 
    filter(`Device Timestamp` > day_start & `Device Timestamp` < day_end) %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    max()
  
  min_value_night <- plot_data %>% 
    filter(`Device Timestamp` < day_start | `Device Timestamp` > day_end) %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    min()
  median_value_night <- plot_data %>% 
    filter(`Device Timestamp` < day_start | `Device Timestamp` > day_end) %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    median()
  mean_value_night <- plot_data %>% 
    filter(`Device Timestamp` < day_start | `Device Timestamp` > day_end) %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    mean()
  q95_value_night <- plot_data %>% 
    filter(`Device Timestamp` < day_start | `Device Timestamp` > day_end) %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    quantile(0.95)
  max_value_night <- plot_data %>% 
    filter(`Device Timestamp` < day_start | `Device Timestamp` > day_end) %>% 
    pull(`Historic Glucose mmol/L`) %>% 
    max()
  
  layout_columns(
    col_widths = rep(4L, 3L),
    card(
      h2("Overall"),
      tags$ul(
        tags$li("Minimum: ", min_value),
        tags$li("Mean: ", format(mean_value, digits = 2)),
        tags$li("Median: ", format(median_value, digits = 2)),
        tags$li("95% quantile: ", format(q95_value, digits = 2, nsmall = 1)),
        tags$li("Maximum: ", max_value)
      )
    ),
    card(
      h2(emoji_glue("Day :sun:")),
      tags$ul(
        tags$li("Minimum: ", min_value_day),
        tags$li("Mean: ", format(mean_value_day, digits = 2)),
        tags$li("Median: ", format(median_value_day, digits = 2)),
        tags$li("95% quantile: ", format(q95_value_day, digits = 2, nsmall = 1)),
        tags$li("Maximum: ", max_value_day)
      )
    ),
    card(
      h2(emoji_glue("Night :crescent_moon:")),
      tags$ul(
        tags$li("Minimum: ", min_value_night),
        tags$li("Mean: ", format(mean_value_night, digits = 2)),
        tags$li("Median: ", format(median_value_night, digits = 2)),
        tags$li("95% quantile: ", format(q95_value_night, digits = 2, nsmall = 1)),
        tags$li("Maximum: ", max_value_night)
      )
    )
  )
}