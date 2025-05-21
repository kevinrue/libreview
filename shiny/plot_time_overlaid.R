plot_time_overlaid <- function(glucose_data, date_annotations, day_type) {
  plot_data <- glucose_data %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    separate(`Device Timestamp`, c("Date", "Time"), sep = " ") %>% 
    left_join(date_annotations, c("Date" = "date")) %>% 
    filter(type == day_type) %>% 
    mutate(
      `Device Timestamp` = as_datetime(ymd_hm(paste(Sys.Date(), Time)))
    ) %>% 
    filter(!is.na(`Historic Glucose mmol/L`))
  ggplot(plot_data) +
    facet_wrap(~type, ncol = 1) +
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
      ymin = 3, ymax = 10,
      fill = "palegreen", alpha = 0.5) +
    geom_line(aes(`Device Timestamp`, `Historic Glucose mmol/L`, group = Date)) +
    scale_x_datetime(
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
}