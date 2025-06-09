plot_histogram_recent <- function(glucose_data, config, recent_days) {
  if (is.null(glucose_data)) {
    return(NULL)
  }
  plot_data <- glucose_data %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    filter(!is.na(`Historic Glucose mmol/L`)) %>% 
    mutate(
      `Device Timestamp` = as_datetime(dmy_hm(`Device Timestamp`))
    ) %>% 
    filter(`Device Timestamp` >= ymd_hms(max(`Device Timestamp`)) - recent_days * 60 * 60 * 24) %>% 
    mutate(
      weekend = factor(
        wday(`Device Timestamp`, label = TRUE) %in% c("Sat", "Sun"),
        c(FALSE, TRUE),
        c("Mon-Fri", "Sat-Sun")
      )
    )
  ggplot(plot_data) +
    annotate(
      geom = "rect",
      xmin = config$target$min,
      xmax = config$target$max,
      ymin = -Inf, ymax = Inf,
      fill = "palegreen", alpha = 0.5) +
    geom_histogram(
      aes(`Historic Glucose mmol/L`),
      linewidth = 0.1,
      binwidth = 0.5,
      boundary = 0,
      colour = "black", fill = "grey"
    ) +
    facet_wrap(~weekend, nrow = 1) +
    scale_x_continuous(limits = c(0, 21), breaks = seq(0, 21, 3)) +
    labs(
      y = "Measurements"
    ) +
    theme_bw() +
    theme(
      panel.grid.minor.x = element_blank()
    ) +
    theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
}

# plot_histogram_all(glucose_data_historic, config)
