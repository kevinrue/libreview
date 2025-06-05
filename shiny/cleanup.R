add_missing_date_annotations <- function(glucose_data, date_annotations) {
  glucose_data_dates <- unique(str_sub(glucose_data$historic$`Device Timestamp`, 1L, 10L))
  missing_dates <- setdiff(glucose_data_dates, date_annotations$date)
  date_annotations <- bind_rows(
    date_annotations,
    tibble(
      date = missing_dates,
      type = "NA"
    )
  ) %>% 
    arrange(date)
}

refactor_na_last <- function(x) {
  unique_values <- sort(unique(x))
  levels_not_na <- setdiff(unique_values, "NA")
  x <- factor(x, c(levels_not_na, "NA"))
  x
}
