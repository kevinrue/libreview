count_alerts <- function(date_annotations, date_type_colors) {
  n_alerts <- 0L
  if (all(date_annotations$type == "NA")) {
    n_alerts <- n_alerts + 1L
  }
  if (is.null(date_type_colors)) {
    n_alerts <- n_alerts + 1L
  }
  return(n_alerts)
}
