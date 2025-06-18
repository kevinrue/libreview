count_alerts <- function(date_annotations, custom_date_type_colors) {
  n_alerts <- 0L
  if (all(date_annotations$type == "NA")) {
    n_alerts <- n_alerts + 1L
  } else {
    if (!custom_date_type_colors) {
      n_alerts <- n_alerts + 1L
    }
  }
  return(n_alerts)
}
