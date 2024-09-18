g_routing_path <- system.file("extdata", "routing.zip", 
                              package = "tidytransit")

test_that("read_gtfs() imports a local file to a list of 
          dataframes and doesnt delete the source file", {

  gtfs_obj = read_gtfs(g_routing_path)
  
  expect_type(gtfs_obj, "list")
  expect_s3_class(gtfs_obj, "tidygtfs")
  expect_s3_class(gtfs_obj, "gtfs")
  expect_true(file.exists(g_routing_path))
})

test_that("reading a real feed", {
  skip_on_cran()
  g_nyc_path = system.file("extdata", "google_transit_nyc_subway.zip", 
                           package = "tidytransit")
  gtfs_obj = read_gtfs(g_nyc_path)
  
  expect_type(gtfs_obj, "list")
  expect_s3_class(gtfs_obj, "tidygtfs")
  expect_s3_class(gtfs_obj, "gtfs")
})

test_that("loud read_gtfs", {
  expect_s3_class(
    read_gtfs(g_routing_path, quiet = FALSE),
    "tidygtfs")
})

test_that("gtfsio arguments", {
  skip_on_cran()
  expect_s3_class(
    read_gtfs(g_routing_path, encoding = "UTF-8"),
    "tidygtfs"
  )
})

test_that("tidygtfs class inheritance list", {
  expect_equal(
    class(read_gtfs(g_routing_path)),
    c("tidygtfs", "gtfs", "list")
  )
})

test_that("the read_gtfs function works with urls", {
  skip_on_cran()
  gtfs_example_url <- 
    "https://github.com/r-transit/tidytransit/raw/master/inst/extdata/sample-feed-fixed.zip"
  
  x <- read_gtfs(gtfs_example_url, quiet=TRUE)
  expect_type(x, "list")
  expect_s3_class(x, "gtfs")
  expect_s3_class(x, "tidygtfs")
})

test_that("the read_gtfs function fails gracefully on bad urls", {
  skip_on_cran()
  
  bad_file <- "/Users/wrong.zip"
  bad_url <- "https://developers.google.com/transit/gtfs/examples/sample-feed-bad.zip"
  
  expect_error(read_gtfs(bad_file), "'path' points to non-existent file: '/Users/wrong.zip'")
  expect_error(suppressWarnings(read_gtfs(bad_url)),
               "cannot open URL 'https://developers.google.com/transit/gtfs/examples/sample-feed-bad.zip'")
})

test_that("Files with BOM can be read", {
  skip_on_cran()
  bom_path <- system.file("extdata", 
              "sample-feed-bom.zip", 
              package = "tidytransit")
  g <- read_gtfs(bom_path)
  expect_s3_class(g, "tidygtfs")
})

test_that("Feed with additional data can be read", {
  g_plus_path <- system.file("extdata", "sample-feed-plus.zip", package = "tidytransit")
  expect_warning(read_gtfs(g_plus_path), "gtfsio/empty_file.txt' has size 0. Returning a NULL data.table.")
  expect_warning(read_gtfs(g_plus_path), "No valid dates defined in feed")
})

test_that("files parameter", {
  path = system.file("extdata", "sample-feed-fixed.zip", package = "tidytransit")

  file_status = unlist(lapply(gtfs_reference, `[[`, "File_Presence"))

  req_files = names(file_status)[file_status %in% c("Required", "Conditionally Required")]
  req_files <- req_files[!req_files %in% c("feed_info", "levels")]
  
  g1 = read_gtfs(path)
  g2 = read_gtfs(path, files = req_files)
  expect_equal(setdiff(names(g1), names(g2)),
               c("fare_attributes", "fare_rules", "frequencies","shapes"))
  
  fns = names(g1)[names(g1) != "." & names(g1) != "calendar_dates"]
  
  for(f in fns) {
    expect_no_warning(read_gtfs(path, files = f)) # no warning expected
  }
})

test_that("NA times", {
  g = read_gtfs(system.file("extdata", "routing-NA-times.zip", package = "tidytransit"))
  
  expect_equal(g$stop_times$arrival_time[c(15,16,18)], rep(hms::hms(NA), 3))
  expect_equal(g$stop_times$departure_time[c(23,19,4)], rep(hms::hms(NA), 3))
})

test_that("non-unique stop_ids", {
  g1 = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  g1$stops$stop_id[1] <- "stop1a"
  g1$trips$trip_id[2] <- "routeA1"
  
  tmppath = tempfile(fileext = ".zip")
  write_gtfs(g1, tmppath)
  
  expect_warning(read_gtfs(tmppath), "Duplicated ids found in: stops, trips")
  
  g2 = suppressWarnings(read_gtfs(tmppath))
  
  expect_s3_class(g2, "gtfs")
  expect_false(inherits(g2, "tidygtfs"))
})
