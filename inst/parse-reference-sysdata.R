library(dplyr)

gtfs_reference_fields = lapply(gtfsio::gtfs_reference, `[[`, "fields") |> 
  bind_rows(.id = "file") |> 
  as_tibble()

reference_date_fields$Type[startsWith(reference_date_fields$Type, "Foreign ID")] <- "ID"
reference_date_fields$Type[startsWith(reference_date_fields$Type, "Unique ID")] <- "ID"

gtfs_reference_types <- distinct(gtfs_reference_fields, file, Field_Name, Type)
gtfs_reference_types <- split(gtfs_reference_types, gtfs_reference_types$Type)

usethis::use_data(
  gtfs_reference_types,
  internal = TRUE, overwrite = TRUE)
