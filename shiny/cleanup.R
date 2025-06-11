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


add_missing_date_type_colors <- function(date_type_colors, date_annotations) {
  if (!is.null(date_type_colors)) {
    if (!all(date_annotations$type %in% names(date_type_colors))) {
      stop(paste0(
        "Colours supplied do not include all types of dates found in annotations:\n",
        paste(setdiff(date_annotations$type, names(date_type_colors)), collapse = ", ")
      ))
    } else {
      return(date_type_colors)
    }
  }
  if (all(date_annotations$type == "NA")) {
    return(NULL)
  }
  unique_day_types_noNA <- setdiff(unique(date_annotations$type), "NA")
  n_types_noNA <- length(unique_day_types_noNA)
  new_colors <-  c(palette(rainbow(n_types_noNA)), "grey")
  names(new_colors) <- c(unique_day_types_noNA, "NA")
  return(new_colors)
}
