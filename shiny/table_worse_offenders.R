top_events <- glucose_data$historic %>% 
  slice_max(`Historic Glucose mmol/L`, prop = 0.1) %>% 
  select(
    `Device Timestamp`,
    `Historic Glucose mmol/L`
  ) %>% 
  rename(
    event_timestamp = `Device Timestamp`
  )

bind_cols(
  top_events,
  latest_note_before_datetime(
    datetime = dmy_hm(top_events$event_timestamp),
    notes = glucose_data$notes %>% mutate(`Device Timestamp` = dmy_hm(`Device Timestamp`))
  )
) %>% 
  select(
    `event_timestamp`,
    `Historic Glucose mmol/L`,
    note_timestamp,
    Notes
  ) %>% 
  distinct(
    note_timestamp,
    .keep_all = TRUE
  ) %>% 
  arrange(desc(`Historic Glucose mmol/L`))

latest_note_before_datetime <- function(datetimes, notes) {
  res <- lapply(datetimes, function(datetime){
    notes %>% 
      filter(`Device Timestamp` < datetime) %>% 
      slice_max(`Device Timestamp`, n = 1) %>% 
      select(`Device Timestamp`, Notes) %>% 
      rename(note_timestamp = `Device Timestamp`)
  })
  do.call("bind_rows", res)
}


latest_note_before_datetime(
  datetime = dmy_hm(top_events$`Device Timestamp`),
  notes = glucose_data$notes %>% mutate(`Device Timestamp` = lubridate::dmy_hm(`Device Timestamp`))
) %>% 
  distinct()
