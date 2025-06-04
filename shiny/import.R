import_glucose_data <- function() {
  glucose_data_all <- do.call(
    "rbind",
    lapply(
      list.files("../data/glucose/", full.names = TRUE),
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
