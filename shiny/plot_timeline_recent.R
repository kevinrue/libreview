plot_timeline_recent <- function(glucose_data, config, recent_days, highlight_weekends, click_datetime) {
  if (is.null(glucose_data)) {
    return(NULL)
  }
  plot_data <- glucose_data$historic %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    mutate(
      `Device Timestamp` = as_datetime(dmy_hm(`Device Timestamp`))
    ) %>% 
    filter(!is.na(`Historic Glucose mmol/L`)) %>% 
    filter(`Device Timestamp` >= ymd_hms(max(`Device Timestamp`)) - recent_days * 60 * 60 * 24)
  nights_start <- ymd_hm(paste(
    seq(
      as.Date(min(plot_data$`Device Timestamp`)) - 1,
      as.Date(max(plot_data$`Device Timestamp`)),
      1
    ),
    "22:00"
  ))
  nights_end <- ymd_hm(paste(
    seq(
      as.Date(min(plot_data$`Device Timestamp`)),
      as.Date(max(plot_data$`Device Timestamp`)) + 1,
      1
    ),
    "06:00"
  ))
  if (min(plot_data$`Device Timestamp`) > nights_end[1]) {
    nights_start <- nights_start[-1]
    nights_end <- nights_end[-1]
  }
  if (max(plot_data$`Device Timestamp`) < nights_start[length(nights_start)]) {
    nights_start <- nights_start[-length(nights_start)]
    nights_end <- nights_end[-length(nights_end)]
  }
  weekends_start <- ymd_hm(paste(
    seq(
      as.Date(min(plot_data$`Device Timestamp`)),
      as.Date(max(plot_data$`Device Timestamp`)),
      1
    ),
    "06:00"
  ))
  weekends_end <- ymd_hm(paste(
    seq(
      as.Date(min(plot_data$`Device Timestamp`)),
      as.Date(max(plot_data$`Device Timestamp`)),
      1
    ),
    "22:00"
  ))
  weekends_start <- weekends_start[which(wday(weekends_start, label = TRUE) %in% c("Sat", "Sun"))]
  weekends_end <- weekends_end[which(wday(weekends_end, label = TRUE) %in% c("Sat", "Sun"))]
  if (length(weekends_end) && min(plot_data$`Device Timestamp`) > weekends_end[1]) {
    weekends_start <- weekends_start[-1]
    weekends_end <- weekends_end[-1]
  }
  if (length(weekends_start) && max(plot_data$`Device Timestamp`) < weekends_start[length(weekends_start)]) {
    weekends_start <- weekends_start[-length(weekends_start)]
    weekends_end <- weekends_end[-length(weekends_end)]
  }
  gg <- ggplot() +
    annotate(
      geom = "rect",
      xmin = nights_start,
      xmax = nights_end,
      ymin = -Inf, ymax = Inf,
      fill = "lightgrey", alpha = 0.5)
  if (highlight_weekends & length(weekends_start) > 0L) {
    gg <- gg + annotate(
      geom = "rect",
      xmin = weekends_start,
      xmax = weekends_end,
      ymin = -Inf, ymax = Inf,
      fill = "orange", alpha = 0.5)
  }
  gg <- gg +
    annotate(
      geom = "rect",
      xmin = min(plot_data$`Device Timestamp`),
      xmax = max(plot_data$`Device Timestamp`),
      ymin = config$target$min, ymax = config$target$max,
      fill = "palegreen", alpha = 0.5) +
    geom_line(
      mapping = aes(`Device Timestamp`, `Historic Glucose mmol/L`),
      data = plot_data,
      linewidth = 0.25
    )
  if (!is.null(click_datetime)) {
    nearest_note <- glucose_data$notes %>%
      mutate(
        `Device Timestamp` = as_datetime(dmy_hm(`Device Timestamp`))
      ) %>%
      mutate(
        abs_diff = abs(as.numeric(`Device Timestamp` - click_datetime))
      ) %>%
      slice_min(abs_diff)
    gg <- gg +
      geom_dotplot(
        mapping = aes(
          x = `Device Timestamp`,
          colour = selected,
          fill = selected,
          stroke = as.integer(selected * 3) + 1
        ),
        y = 0,
        data = glucose_data$notes %>%
          mutate(
            `Device Timestamp` = as_datetime(dmy_hm(`Device Timestamp`)),
            selected = as.numeric(`Device Timestamp`) == as.numeric(nearest_note$`Device Timestamp`)
          ),
        binwidth = 60 * 60,
        stackgroups = TRUE,
        binpositions = "all"
      )
    gg <- gg +
      geom_label(
        mapping = aes(x, y, label = label),
        data = tibble(
          x = as_datetime(click_datetime),
          y = 20,
          label = paste0(
            str_sub(as.character(nearest_note$`Device Timestamp`), 12L, 16L), "\n",
            nearest_note$Notes
          )
        )
      )
  } else {
    gg <- gg +
      geom_dotplot(
        mapping = aes(x = `Device Timestamp`),
        y = 0,
        data = glucose_data$notes %>% mutate(
          `Device Timestamp` = as_datetime(dmy_hm(`Device Timestamp`))
        ),
        binwidth = 60 * 60
      )
  }
  gg <- gg + scale_x_datetime(
    expand = c(0, 0, 0, 0),
    oob = scales::squish_infinite, date_breaks = "day", date_labels = "%a\n%d %b"
  ) +
    scale_y_continuous(breaks = seq(0, 21, 3)) +
    scale_fill_manual(
      values = c("TRUE" = "red", "FALSE" = "black"),
      aesthetics = c("colour", "fill")
    ) +
    guides(
      fill = "none", colour = "none"
    ) +
    coord_cartesian(
      xlim = range(plot_data$`Device Timestamp`),
      ylim = c(0, 21)
    ) +
    theme(
      axis.text.x = element_text(angle = 90)
    ) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  gg
}

## Test ----

# glucose_data <- import_glucose_data(default_glucose_files)
# 
# config <- yaml::read_yaml("config.yaml")
# 
# recent_days <- 7L
# 
# highlight_weekends <- TRUE
# 
# click_datetime <- 1749126600
# 
# plot_timeline_recent(glucose_data, config, recent_days, highlight_weekends, click_datetime)
