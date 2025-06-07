cluster_time_all <- function(glucose_data) {
  if (is.null(glucose_data)) {
    return(NULL)
  }
  keep_dates <- glucose_data %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    filter(!is.na(`Historic Glucose mmol/L`)) %>% 
    mutate(
      `Device Timestamp` = as_datetime(dmy_hm(`Device Timestamp`))
    ) %>% 
    separate(`Device Timestamp`, c("Date", "Time"), sep = " ") %>% 
    group_by(Date) %>% 
    summarise(n_obs = n()) %>% 
    filter(n_obs > 50) %>% 
    pull(Date)
  plot_data <- glucose_data %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    filter(!is.na(`Historic Glucose mmol/L`)) %>% 
    mutate(
      `Device Timestamp` = round_hms(as_datetime(dmy_hm(`Device Timestamp`)), 15*60)
    ) %>% 
    mutate(
      `Device Timestamp` = format_ISO8601(`Device Timestamp`)
    ) %>% 
    separate(`Device Timestamp`, c("Date", "Time"), sep = "T") %>% 
    filter(Date %in% keep_dates) %>% 
    pivot_wider(names_from = `Time`, values_from = `Historic Glucose mmol/L`, values_fn = mean) %>% 
    arrange(`Date`) %>% 
    select(order(colnames(.))) %>% 
    select(c(Date, contains(":")))
  dist_data <- dist(plot_data %>% column_to_rownames("Date"))
  hclust_data <- hclust(dist_data)
  plot(hclust_data)
}

heatmap_time_all <- function(glucose_data, date_annotations, recent_days, cluster_days = FALSE) {
  if (is.null(glucose_data)) {
    return(NULL)
  }
  keep_dates <- glucose_data %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    filter(!is.na(`Historic Glucose mmol/L`)) %>% 
    mutate(
      `Device Timestamp` = as_datetime(dmy_hm(`Device Timestamp`))
    ) %>% 
    separate(`Device Timestamp`, c("Date", "Time"), sep = " ") %>% 
    filter(
      as_hms(Time) >= as_hms("06:00:00") & as_hms(Time) <= as_hms("22:00:00")
    ) %>% 
    group_by(Date) %>% 
    summarise(n_obs = n()) %>% 
    filter(n_obs > 50) %>% 
    pull(Date)
  min_date_keep <- as.Date(max(dmy_hm(glucose_data$`Device Timestamp`)) - recent_days * 60 * 60 * 24)
  keep_dates <- keep_dates[as.Date(keep_dates) >= min_date_keep]
  plot_data <- glucose_data %>% 
    select(`Device Timestamp`, `Historic Glucose mmol/L`) %>% 
    filter(!is.na(`Historic Glucose mmol/L`)) %>% 
    mutate(
      `Device Timestamp` = round_hms(as_datetime(dmy_hm(`Device Timestamp`)), 15*60)
    ) %>% 
    mutate(
      `Device Timestamp` = format_ISO8601(`Device Timestamp`)
    ) %>% 
    separate(`Device Timestamp`, c("Date", "Time"), sep = "T") %>% 
    filter(Date %in% keep_dates) %>% 
    pivot_wider(names_from = `Time`, values_from = `Historic Glucose mmol/L`, values_fn = mean) %>% 
    arrange(`Date`) %>% 
    select(order(colnames(.))) %>% 
    column_to_rownames("Date") %>% 
    as.matrix()
  plot_row_group <- tibble(
    date = rownames(plot_data)
  ) %>% 
    mutate(
      weekend = factor(
        wday(date, label = TRUE) %in% c("Sat", "Sun"),
        c(FALSE, TRUE),
        c("Mon-Fri", "Sat-Sun")
      )
    )
  if (!all(date_annotations$type == "NA")) {
    plot_row_group <- plot_row_group %>% left_join(
      date_annotations %>%
        mutate(
          date = format_ISO8601(as.Date(as_date(dmy(date))))
          ),
      by = "date")
  }
  plot_row_group <- plot_row_group %>%
    column_to_rownames("date")
  plot_column_group <- tibble(
    time = sort(unique(colnames(plot_data))),
    datetime = as_datetime(ymd_hms(paste(Sys.Date(), time))),
    phase = factor(ifelse(
      test = datetime > ymd_hm(paste(Sys.Date(), "06:00")) & datetime < ymd_hm(paste(Sys.Date(), "22:00")),
      yes = "day",
      no = "night"
    ), levels = c("day", "night")),
    tick = factor(ifelse(time %in% c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:45:00"), yes = "6h", no = "other"))
  ) %>% 
    column_to_rownames("time") %>% 
    select(phase, tick)
  pheatmap(
    mat = plot_data,
    color = colorRampPalette(rev(brewer.pal(n = 11, name = "Spectral")))(100),
    breaks = seq(3.9, 13.3, length.out = 100),
    annotation_row = plot_row_group,
    annotation_col = plot_column_group,
    annotation_names_col = FALSE,
    annotation_names_row = FALSE,
    cluster_rows = cluster_days,
    cluster_cols = FALSE,
    show_colnames = FALSE,
    annotation_colors = list(
      tick = c("6h" = "black", "other" = "white"),
      phase = c("day" = "yellow", "night" = "darkblue"),
      weekend = c("Mon-Fri" = "white", "Sat-Sun" = "orange")
    ))
}
