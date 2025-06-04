.all_label <- "__All__"

glucose_file_spec <- cols(
  Device = col_character(),
  `Serial Number` = col_character(),
  `Device Timestamp` = col_character(),
  `Record Type` = col_factor(),
  `Historic Glucose mmol/L` = col_double(),
  `Scan Glucose mmol/L` = col_double(),
  `Non-numeric Rapid-Acting Insulin` = col_logical(),
  `Rapid-Acting Insulin (units)` = col_double(),
  `Non-numeric Food` = col_logical(),
  `Carbohydrates (grams)` = col_double(),
  `Carbohydrates (servings)` = col_logical(),
  `Non-numeric Long-Acting Insulin` = col_logical(),
  `Long-Acting Insulin Value (units)` = col_double(),
  Notes = col_character(),
  `Strip Glucose mmol/L` = col_double(),
  `Ketone mmol/L` = col_double(),
  `Meal Insulin (units)` = col_double(),
  `Correction Insulin (units)` = col_double(),
  `User Change Insulin (units)` = col_double()
)
