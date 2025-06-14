default_glucose_files <- list.files("../data/glucose/", pattern = "*.csv$", full.names = TRUE)
default_date_annotations_file <- "../data/date_annotations.csv"
default_day_type_file <- "../data/date_type_colors.csv"

import_glucose_data <- function(glucose_files) {
  if(length(glucose_files) < 1L) {
    return(NULL)
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

import_date_annotations <- function(date_annotations_file) {
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

import_date_type_colors <- function(date_type_file) {
  if (file.exists(date_type_file)) {
    date_colors <- read_csv(
      file = date_type_file,
      col_names = c("date", "color"),
      show_col_types = FALSE
    ) %>% 
      mutate(
        date = replace_na(date, "NA")
      ) %>%
      deframe()
  } else {
    NULL
  }
}
