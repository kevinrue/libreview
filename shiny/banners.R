banner_heatmap_time_recent <- function(date_annotations, custom_date_type_colors) {
  show_banner <- FALSE
  if (all(date_annotations$type == "NA")) {
    show_banner <- TRUE
  }
  if (!custom_date_type_colors) {
    show_banner <- TRUE
    # card(
    #   style="color:#8a8a86;background-color:#f3f593;",
    #   height = "60px",
    #   p(
    #     emoji("light bulb"),
    #     em("Tip: Default colour scheme used for day types."),
    #     em("Add a file"),
    #     tags$code("../data/data_type_colors.csv"),
    #     em("to create a custom colour scheme.")
    #   )
    # )
  }
  if (show_banner) {
    return(card(
      style="color:#8a8a86;background-color:#f3f593;",
      height = "60px",
      p(
        emoji("light bulb"),
        em("Tip: You can enhance this plot! Click the notification icon to know more.")
      )
    ))
  } else {
    return(NULL)
  }
}

banner_plot_timeline_overlaid <- function(date_annotations, custom_date_type_colors) {
  show_banner <- FALSE
  if (all(date_annotations$type == "NA")) {
    show_banner <- TRUE
  }
  if (!custom_date_type_colors) {
    show_banner <- TRUE
    # card(
    #   style="color:#8a8a86;background-color:#f3f593;",
    #   height = "60px",
    #   p(
    #     emoji("light bulb"),
    #     em("Tip: Default colour scheme used for day types."),
    #     em("Add a file"),
    #     tags$code("../data/data_type_colors.csv"),
    #     em("to create a custom colour scheme.")
    #   )
    # )
  }
  if (show_banner) {
    return(card(
      style="color:#8a8a86;background-color:#f3f593;",
      height = "60px",
      p(
        emoji("light bulb"),
        em("Tip: You can enhance this plot! Click the notification icon to know more.")
      )
    ))
  } else {
    return(NULL)
  }
}
