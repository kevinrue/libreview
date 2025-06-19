compute_pca <- function(glucose_data, recent_days = 14, method = "bpca") {
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
  pca_method <- get(method, "package:pcaMethods")
  pca_out <- pca_method(plot_data, nPcs = 2)
  if (method == "bpca") {
    out <- pca_out@scores
    rownames(out) <- rownames(plot_data)
    return(out)
  } else if (method == "ppca") {
    return(pca_out@scores)
  } else {
    stop("Method not recognised")
  }
}

# pca_out <- compute_pca(glucose_data, 14, method = "ppca")
# 
# plot_data <- pca_out %>%
#   as.data.frame() %>%
#   rownames_to_column("date") %>%
#   mutate(date = format(ymd(date), "%d-%m-%Y")) %>%
#   left_join(date_annotations, by = "date")
# 
# ggplot(plot_data, aes(V1, V2, colour = type)) +
#   geom_point(size = 3) +
#   theme_minimal()
# 
# result <- pca(plot_data, method="ppca", nPcs=3, seed=123)
# cObs <- completeObs(result)
# 
# 
# plot_pca

# na_to_row_columns <- function(plot_data) {
#   idx <- which(is.na(plot_data))
#   idx_col <- (idx %/% nrow(plot_data)) + 1
#   idx_row <- idx - ((idx_col-1) * nrow(plot_data))
#   idx_col <- ifelse(idx_row == 0, idx_col - 1, idx_col)
#   idx_row <- ifelse(idx_row == 0, nrow(plot_data), idx_row)
# }

#recent_days <- 30
