library(lubridate)
library(readr)
current_data <- read_csv("../data/date_annotations.csv", col_names = c("date", "type"))
# tail(current_data)
latest_annotated_date <- max(dmy(current_data$date))
today <- ymd(Sys.Date())
missing_dates <- as_date((latest_annotated_date+1):today)
missing_dates_formatted <- format(missing_dates, format = "%d-%m-%Y")
new_data <- bind_rows(
  current_data,
  tibble(
    date = missing_dates_formatted,
    type = "NA"
  )
)
write_csv(new_data, "../data/date_annotations.csv", col_names = FALSE)
