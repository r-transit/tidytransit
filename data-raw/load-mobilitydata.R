tmpfile = tempfile(fileext = ".csv")
curl::curl_download("https://storage.googleapis.com/storage/v1/b/mdb-csv/o/sources.csv?alt=media", tmpfile)

csv = read.csv(tmpfile)

mobilitydata = csv |> 
  dplyr::filter(data_type == "gtfs") |> 
  dplyr::select(mdb_source_id, data_type, provider, urls.direct_download,
                location.bounding_box.minimum_longitude,
                location.bounding_box.maximum_longitude,
                location.bounding_box.minimum_latitude,
                location.bounding_box.maximum_latitude) |> 
  dplyr::mutate(location.bounding_box.minimum_longitude = round(location.bounding_box.minimum_longitude, 5),
                location.bounding_box.maximum_longitude = round(location.bounding_box.maximum_longitude, 5),
                location.bounding_box.minimum_latitude = round(location.bounding_box.minimum_latitude, 5),
                location.bounding_box.maximum_latitude = round(location.bounding_box.maximum_latitude, 5))

saveRDS(mobilitydata, "../inst/extdata/mobilitydata.rds", compress = "xz")
