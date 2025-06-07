import_glucose_data <- function() {
  glucose_files <- list.files("../data/glucose/", pattern = "*.csv$", full.names = TRUE)
  if(length(glucose_files) < 1L) {
    stop("No CSV file found under '../data/glucose/'")
  }
  glucose_data_all <- do.call(
    "rbind",
    lapply(
      glucose_files,
      read_csv,
      skip = 1,
      col_types = glucose_file_spec
    )
  ) %>% 
    distinct()
  
  glucose_data_historic <- glucose_data_all %>% 
    filter(
      `Record Type` %in% c(0)
    ) %>% 
    select(Device, `Serial Number`, `Device Timestamp`, `Historic Glucose mmol/L`)
  
  glucose_data_scan <- glucose_data_all %>% 
    filter(
      `Record Type` %in% c(1)
    ) %>% 
    select(Device, `Serial Number`, `Device Timestamp`, `Scan Glucose mmol/L`)
  
  glucose_data_notes <- glucose_data_all %>% 
    filter(
      `Record Type` %in% c(6),
      !is.na(Notes)
    ) %>% 
    select(Device, `Serial Number`, `Device Timestamp`, `Notes`)
  
  return(
    list(
      historic = glucose_data_historic,
      scan = glucose_data_scan,
      notes = glucose_data_notes
    )
  )
}

import_date_annotations <- function() {
  date_annotations_file <- "../data/date_annotations.csv"
  if (file.exists(date_annotations_file)) {
    date_annotations <- read_csv(
      file = date_annotations_file,
      col_names = c("date", "type"),
      show_col_types = FALSE
    ) %>% 
      mutate(
        type = replace_na(type, "NA")
      )
  } else {
    tibble(date = character(0), type = character(0))
  }
}
