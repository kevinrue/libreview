banner_missing_custom_date_type_colours <- function(date_annotations, custom_date_type_colors) {
  if (!all(date_annotations$type == "NA") && !custom_date_type_colors) {
    card(
      style="color:#8a8a86;background-color:#f3f593;",
      height = "60px",
      p(
        emoji("light bulb"),
        em("Tip: Default colour scheme used for day types."),
        em("Add a file"),
        tags$code("../data/data_type_colors.csv"),
        em("to create a custom colour scheme.")
      )
    )
  }
}
