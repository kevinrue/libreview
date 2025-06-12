nearest_datetime <- function(query, x) {
  .local <- function(q, x) {
    which.min(abs(as.numeric(q - x)))
  }
  vapply(query, .local, integer(1), x = x)
}
