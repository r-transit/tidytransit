routing.zip = system.file("extdata", "routing.zip", package = "tidytransit")
tidygtfs = read_gtfs(routing.zip)

test_that("as_tidygtfs w/ gtfstools", {
  dt_gtfs = readRDS(system.file("extdata", "routing.zip_gtfstools.rds", package = "tidytransit"))
  expect_equal(class(dt_gtfs), c("dt_gtfs", "gtfs", "list"))
  
  dt_gtfs.tidygtfs = as_tidygtfs(dt_gtfs)
  expect_equal(names(dt_gtfs.tidygtfs), names(tidygtfs))
  expect_equal(names(dt_gtfs.tidygtfs$.), names(tidygtfs$.))
  
  for(table_name in names(dt_gtfs.tidygtfs)) {
    expect_equal(dt_gtfs.tidygtfs[[table_name]], tidygtfs[[table_name]])
  }
  
  expect_s3_class(dt_gtfs.tidygtfs, "tidygtfs")
})

test_that("as_tidygtfs w/ list", {
  gtfs_list <- lapply(tidygtfs, function(y) {
    dplyr::as_tibble(y)
  })
  gtfs_list$. <- NULL
  gtfs_list$stop_times$departure_time <- as.character(gtfs_list$stop_times$departure_time)
  
  expect_equal(class(gtfs_list), "list")
  
  gtfs_list.tidygtfs = as_tidygtfs(gtfs_list)
  
  expect_equal(gtfs_list.tidygtfs, tidygtfs)
  expect_s3_class(gtfs_list.tidygtfs, "tidygtfs")
})

test_that("as_tidygtfs w/ gtfs", {
  gtfs = gtfsio::import_gtfs(routing.zip)
  expect_equal(class(gtfs), c("gtfs", "list"))
  
  gtfs.tidygtfs = as_tidygtfs(gtfs)
  
  expect_equal(gtfs.tidygtfs, tidygtfs)
  expect_s3_class(gtfs.tidygtfs, "tidygtfs")
})

test_that("as_tidygtfs w/ tidygtfs", {
  x1 = read_gtfs(routing.zip)
  x1$calendar <- x1$calendar[1,]
  x1$calendar_dates <- x1$calendar_dates[1:2,]
  
  x2 = as_tidygtfs(x1)
  
  expect_true(all(x2$.$dates_services$service_id == "WEEK"))
  expect_s3_class(x2, "tidygtfs")
})
