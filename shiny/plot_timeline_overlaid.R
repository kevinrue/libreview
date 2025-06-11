plot_timeline_overlaid <- function(
    glucose_data_historic,
  date_annotations,
  config,
  day_type,
  color_day_type,
  date_type_colors
) {
  plot_data <- glucose_data_historic %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    filter(!is.na(`Historic Glucose mmol/L`)) %>% 
    separate(`Device Timestamp`, c("Date", "Time"), sep = " ") %>% 
    mutate(
      `Device Timestamp` = as_datetime(ymd_hm(paste(Sys.Date(), Time)))
    )
  if (!length(color_day_type)) {
    color_day_type <- FALSE
  }
  if (!is.null(date_annotations)) {
    if (!length(day_type)) {
      day_type <- unique(date_annotations$type)
    }
    plot_data <- plot_data %>% 
      left_join(date_annotations, c("Date" = "date")) %>% 
      filter(type %in% day_type)
  }
  gg <- ggplot(plot_data) +
    annotate(
      geom = "rect",
      xmin = ymd_hm(paste(Sys.Date(), "00:00")),
      xmax = ymd_hm(paste(Sys.Date(), "06:00")),
      ymin = -Inf, ymax = Inf,
      fill = "lightgrey", alpha = 0.5) +
    annotate(
      geom = "rect",
      xmin = ymd_hm(paste(Sys.Date(), "22:00")),
      xmax = ymd_hm(paste(Sys.Date() + 1, "00:00")),
      ymin = -Inf, ymax = Inf,
      fill = "lightgrey", alpha = 0.5) +
    annotate(
      geom = "rect",
      xmin = ymd_hm(paste(Sys.Date(), "00:00")),
      xmax = ymd_hm(paste(Sys.Date() + 1, "00:00")),
      ymin = config$target$min, ymax = config$target$max,
      fill = "palegreen", alpha = 0.5)
  if (color_day_type) {
    gg <- gg +
      geom_line(aes(`Device Timestamp`, `Historic Glucose mmol/L`, group = Date, colour = type)) +
      scale_colour_manual(values = date_type_colors)
  } else {
    gg <- gg + geom_line(aes(`Device Timestamp`, `Historic Glucose mmol/L`, group = Date))
  }
  gg <- gg + scale_x_datetime(
      date_label = "%H:%M",
      expand = c(0, 0, 0, 0),
      oob = scales::squish_infinite
    ) +
    scale_y_continuous(limits = c(0, 21), breaks = seq(0, 21, 3)) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
  return(gg)
}

## test ----

# glucose_data <- import_glucose_data(default_glucose_files)
# glucose_data_historic <- glucose_data$historic
# 
# date_annotations <- import_date_annotations(default_date_annotations_file)
# date_annotations <- add_missing_date_annotations(glucose_data, date_annotations) %>%
#   mutate(
#     type = refactor_na_last(type)
#   )
# 
# recent_days <- 30L
# 
# highlight_weekends <- TRUE
# 
# cluster_days <- FALSE
# 
# date_type_colors <- import_date_type_colors(default_day_type_file)
# 
# plot_timeline_overlaid(glucose_data, date_annotations, config, day_type, color_day_type, date_type_colors)
