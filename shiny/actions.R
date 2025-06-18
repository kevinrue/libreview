get_actions_modal_ui <- function(date_annotations, custom_date_type_colors) {
  if (all(date_annotations$type == "NA")) {
    return(tagList(
      p(
        emoji("light bulb"),
        em("Tip: Import date annotations to colour and filter different types of days!"),
        fileInput("date_annotation_file", label = "Date annotations", multiple = FALSE, accept = ".csv")
      )
    ))
  } else {
    if (!custom_date_type_colors) {
      return(p(
        emoji("light bulb"),
        em("Tip: Default colour scheme used for day types."),
        em("Add a file"),
        tags$code("../data/data_type_colors.csv"),
        em("to create a custom colour scheme.")
      ))
    }
  }
}