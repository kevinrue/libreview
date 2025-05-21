plot_time_all <- function(glucose_data) {
  plot_data <- glucose_data %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    mutate(
      `Device Timestamp` = as_datetime(dmy_hm(`Device Timestamp`))
    ) %>% 
    filter(!is.na(`Historic Glucose mmol/L`))
  nights_start <- ymd_hm(paste(seq(as.Date(min(plot_data$`Device Timestamp`))-1, as.Date(max(plot_data$`Device Timestamp`)), 1), "22:00"))
  nights_end <- ymd_hm(paste(seq(as.Date(min(plot_data$`Device Timestamp`)), as.Date(max(plot_data$`Device Timestamp`))+1, 1), "06:00"))
  if (min(plot_data$`Device Timestamp`) > nights_end[1]) {
    nights_start <- nights_start[-1]
    nights_end <- nights_end[-1]
  }
  if (max(plot_data$`Device Timestamp`) < nights_start[length(nights_start)]) {
    nights_start <- nights_start[-length(nights_start)]
    nights_end <- nights_end[-length(nights_end)]
  }
  ggplot(plot_data) +
    annotate(
      geom = "rect",
      xmin = nights_start,
      xmax = nights_end,
      ymin = -Inf, ymax = Inf,
      fill = "lightgrey", alpha = 0.5) +
    annotate(
      geom = "rect",
      xmin = min(plot_data$`Device Timestamp`),
      xmax = max(plot_data$`Device Timestamp`),
      ymin = 3, ymax = 10,
      fill = "palegreen", alpha = 0.5) +
    geom_line(aes(`Device Timestamp`, `Historic Glucose mmol/L`)) +
    scale_x_datetime(
      expand = c(0, 0, 0, 0),
      oob = scales::squish_infinite, date_breaks = "day", date_labels = "%d %b"
    ) +
    scale_y_continuous(limits = c(0, 21), breaks = seq(0, 21, 3)) +
    theme(
      axis.text.x = element_text(angle = 90)
    ) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}