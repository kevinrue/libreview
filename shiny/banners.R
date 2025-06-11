banner_heatmap_time_recent <- function(date_annotations, custom_date_type_colors) {
  banner <- if (all(date_annotations$type == "NA")) {
    card(
      style="color:#8a8a86;background-color:#f3f593;",
      height = "60px",
      p(
        emoji("light bulb"),
        em("Tip: Import date annotations to display them on the side of the heatmap!"),
        actionLink("date_annotation_modal_open", "Click here to import")
      )
    )
  } else if (!custom_date_type_colors) {
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
  } else {
    NULL
  }
  return(banner)
}

banner_plot_timeline_overlaid <- function(date_annotations, custom_date_type_colors) {
  banner <- if (all(date_annotations$type == "NA")) {
    card(
      style="color:#8a8a86;background-color:#f3f593;",
      height = "60px",
      p(
        emoji("light bulb"),
        em("Tip: Import date annotations to color days by type!"),
        actionLink("date_annotation_modal_open", "Click here to import")
      )
    )
  } else if (!custom_date_type_colors) {
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
  } else {
    NULL
  }
}
