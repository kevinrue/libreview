cluster_time_all <- function(glucose_data) {
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

heatmap_time_all <- function(glucose_data, date_annotations) {
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
    column_to_rownames("Date") %>% 
    as.matrix()
  plot_row_group <- date_annotations %>% 
    mutate(
      date = format_ISO8601(as.Date(as_date(dmy(date))))
    ) %>% 
    filter(date %in% keep_dates) %>% 
    column_to_rownames("date")
  pheatmap(
    mat = plot_data,
    annotation_row = plot_row_group,
    cluster_cols = FALSE,
    main = "Clustered daily patterns")
}