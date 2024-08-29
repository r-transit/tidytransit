library(dplyr)

fields = lapply(gtfsio::gtfs_reference, `[[`, "fields") |> 
  bind_rows(.id = "file") |> 
  as_tibble()

reference_date_fields = fields |> distinct(file, Field_Name, Type) |> 
  filter(grepl("Date", Type))

reference_time_fields = fields |> distinct(file, Field_Name, Type) |> 
  filter(Type == "Time")

usethis::use_data(
  reference_date_fields,
  reference_time_fields,
  internal = TRUE, overwrite = TRUE)
