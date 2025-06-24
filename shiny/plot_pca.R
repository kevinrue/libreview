compute_pca <- function(glucose_data, method = "ppca", nPcs = 10) {
  glucose_data_historic <- glucose_data$historic
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
  pca_data <- glucose_data_historic %>% 
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
  pca_out <- pca(pca_data, method = method, center = FALSE, nPcs = nPcs)
  if (method == "bpca") { 
    rownames(pca_out@scores) <- rownames(pca_data)
    out <- pca_out
  } else if (method == "ppca") {
    out <- pca_out
  } else {
    stop("Method not recognised")
  }
  return(pca_out)
}

plot_pca <- function(pca_out, date_annotations, size = 3L) {
  plot_data <- pca_out@scores %>%
    as.data.frame() %>%
    rownames_to_column("date") %>%
    mutate(date = format(ymd(date), "%d-%m-%Y")) %>%
    left_join(date_annotations, by = "date")
  
  label_data <- plot_data %>% 
    group_by(type) %>% 
    summarise(across(PC1:PC2, mean))

  ggplot(plot_data, aes(PC1, PC2, colour = type)) +
    geom_point(size = size) +
    geom_label(aes(label = type), data = label_data, show.legend = FALSE) +
    guides(
      colour = guide_legend(override.aes = list(size = 5L))
    ) +
    theme_minimal()
}

# Testing ----

# pca_out <- compute_pca(glucose_data, 14, method = "ppca")
# 
# plot_data <- pca_out@scores %>%
#   as.data.frame() %>%
#   rownames_to_column("date") %>%
#   mutate(date = format(ymd(date), "%d-%m-%Y")) %>%
#   left_join(date_annotations, by = "date")
# 
# ggplot(plot_data, aes(PC1, PC2, colour = type)) +
#   geom_point(size = 3) +
#   theme_minimal()
