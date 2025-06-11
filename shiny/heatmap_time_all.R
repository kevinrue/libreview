heatmap_time_all <- function(
  glucose_data_historic,
  date_annotations,
  recent_days,
  highlight_weekends,
  cluster_days,
  date_type_colors
) {
  if (is.null(glucose_data_historic)) {
    return(NULL)
  }
  keep_dates <- glucose_data_historic %>% 
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
  min_date_keep <- as.Date(max(dmy_hm(glucose_data_historic$`Device Timestamp`)) - recent_days * 60 * 60 * 24)
  keep_dates <- keep_dates[as.Date(keep_dates) >= min_date_keep]
  plot_data <- glucose_data_historic %>% 
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
  )
  row_annot_colors <- list()
  if (highlight_weekends) {
    plot_row_group <- plot_row_group %>% 
      mutate(
        weekend = factor(
          wday(date, label = TRUE) %in% c("Sat", "Sun"),
          c(FALSE, TRUE),
          c("Mon-Fri", "Sat-Sun")
        )
      )
    row_annot_colors[["weekend"]] <- c("Mon-Fri" = "white", "Sat-Sun" = "orange")
  }
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
  if (!is.null(date_type_colors)) {
    row_annot_colors[["type"]] <- date_type_colors
  }
  row_ha <- HeatmapAnnotation(
    which = "row",
    df = plot_row_group,
    col = row_annot_colors, show_annotation_name = FALSE
  )
  column_annotations <- tibble(
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
  column_ha <- HeatmapAnnotation(
    which = "column",
    tick = column_annotations$tick,
    phase = column_annotations$phase,
    col = list(
      tick = c("6h" = "black", "other" = "white"),
      phase = c("day" = "#fafa57", "night" = "#323796")
    ),
    show_annotation_name = FALSE
  )
  rownames(plot_data) <- strftime(as.Date(rownames(plot_data)), format = "%a %d %b")
  rownames(plot_row_group) <- strftime(as.Date(rownames(plot_row_group)), format = "%a %d %b")
  ht_list <- Heatmap(
    matrix = plot_data, name = "Glucose mmol/L",
    col = colorRampPalette(rev(brewer.pal(n = 11, name = "Spectral")))(100),
    top_annotation = column_ha,
    left_annotation = row_ha,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_column_names = FALSE
  )
  draw(ht_list, heatmap_legend_side = "left", annotation_legend_side = "bottom")
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
# heatmap_time_all(glucose_data_historic, date_annotations, recent_days, highlight_weekends, cluster_days, date_type_colors)
