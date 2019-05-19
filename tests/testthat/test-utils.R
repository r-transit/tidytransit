context("Utils")

test_that("write_gtfs creates the same feed as read by read_gtfs", {
  skip_on_cran()
  path1 <- system.file("extdata", "sample-feed-fixed.zip", package = "tidytransit")
  path2 <- tempfile(fileext = ".zip")
  g1 <- read_gtfs(path1, local = TRUE)
  g1_hms <- set_hms_times(g1)
  write_gtfs(g1_hms, path2)
  g2 <- read_gtfs(path2, local = TRUE)
  expect_equal(g1, g2)
  expect_error(write_gtfs(g1$agency, path2))
})
