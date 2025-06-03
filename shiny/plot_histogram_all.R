plot_histogram_all <- function(glucose_data, config) {
  plot_data <- glucose_data %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    filter(!is.na(`Historic Glucose mmol/L`))
  ggplot(plot_data) +
    annotate(
      geom = "rect",
      xmin = config$target$min,
      xmax = config$target$max,
      ymin = -Inf, ymax = Inf,
      fill = "palegreen", alpha = 0.5) +
    geom_histogram(
      aes(`Historic Glucose mmol/L`),
      binwidth = 0.5,
      boundary = 0,
      colour = "black", fill = "grey"
    ) +
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
