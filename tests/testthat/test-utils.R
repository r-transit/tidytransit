context("Utils")

test_that("write_gtfs creates the same feed as read by read_gtfs", {
  skip_on_cran()
  path1 <- system.file("extdata", "sample-feed-fixed.zip", package = "tidytransit")
  path2 <- tempfile(fileext = ".zip")
  g1 <- read_gtfs(path1)
  write_gtfs(g1, path2)
  g2 <- read_gtfs(path2)
  expect_equal(g1, g2)
  expect_error(write_gtfs(g1$agency, path2))
})

test_that("write_gtfs as_dir", {
  skip_on_cran()
  path1 <- system.file("extdata", "sample-feed-fixed.zip", package = "tidytransit")
  path2 <- "_test_tmp"
  g1 <- read_gtfs(path1)
  write_gtfs(g1, path2, as_dir = TRUE)
  expect_true(file.exists(paste0(path2, "/agency.txt")))
  unlink("_test_tmp")
})

test_that("summary.tidygtfs", {
  gpath <- system.file("extdata", "routing.zip", package = "tidytransit")
  g1 = read_gtfs(gpath)
  x1 = capture.output(summary(g1))
  g2 <- set_dates_services(g1)
  x2 = capture.output(summary(g2))
  expect_true(all(x1 == x2))
})

test_that("filter_stops", {
  g = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  fs1 = filter_stops(g, service_ids = "WEEK", route_ids = c("lineA", "lineD"))
  expect_equal(sort(unique(fs1$stop_id)), c("stop1a", "stop1b", "stop2", "stop3a", "stop3b", "stop4", "stop8b"))
  fs2 = filter_stops(g, service_ids = "WEND", route_ids = c("lineA", "lineD"))
  expect_equal(nrow(fs2), 0)
})
