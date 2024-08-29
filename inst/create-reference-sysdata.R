library(dplyr)

gtfs_reference = gtfsio::gtfs_reference

ref_fields = lapply(gtfs_reference, `[[`, "fields") |> 
  bind_rows(.id = "file") |> 
  as_tibble()

ref_fields$Type[startsWith(ref_fields$Type, "Foreign ID")] <- "ID"
ref_fields$Type[startsWith(ref_fields$Type, "Unique ID")] <- "ID"

gtfs_reference_types = distinct(ref_fields, file, Field_Name, Type)
gtfs_reference_types <- split(gtfs_reference_types, gtfs_reference_types$Type)

gtfs_reference_filetype = unlist(lapply(gtfs_reference, `[[`, "file_ext"))

usethis::use_data(
  gtfs_reference,
  gtfs_reference_types,
  gtfs_reference_filetype,
  internal = TRUE, overwrite = TRUE)
