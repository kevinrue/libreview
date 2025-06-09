get_help_modal_ui <- function(nav_item_id) {
  ui <- if (nav_item_id == recent_data_id) {
    tagList(
      h3("Help for", sQuote(recent_data_id)),
      h4("Timeline"),
      p("This plot displays a timeline of historic glucose data for a number of recent days."),
      p(
        "The number of recent days to display can be controlled, from 1 to 30.",
        "This number is relative to the latest day for available data, not the present day."
      ),
      p("Weekend days can be highlighted along the timeline."),
      p("Click anywhere on the plot to reveal the closest note in the timeline."),
      hr(),
      h4("Heatmap"),
      p("This plot aligns historic glucose data for a number of recent days, at 15 minute intervals."),
      p("Black marks above the heatmap indicate 6h intervals."),
      p("Blue and yellow marks above the heatmap indicate approximate day time and night time."),
      p("Orange marks on the side of the heatmap indicate weekends."),
      p(
        "Optionally, on the side of the heatmap, annotations may be added to categorise day types.",
        "By default, this is achieved via a file named",
        tags$code("../data_annotations.csv"),
        "organised as follows:"
      ),
      tags$pre(tags$code(
        "01-01-2000,work",
        "02-01-2000,holiday",
        "03-01-2000,holiday"
      )),
      h4("Histogram"),
      em("Coming soon...")
    )
  } else if (nav_item_id == overlay_days_id) {
    tagList(
      h3("Help for", sQuote(overlay_days_id)),
      em("Coming soon...")
    )
  }
  return(ui)
}