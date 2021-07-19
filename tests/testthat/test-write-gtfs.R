context("Export")

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
  path2 <- tempfile()
  g1 <- read_gtfs(path1)
  write_gtfs(g1, path2, as_dir = TRUE)
  expect_true(file.exists(paste0(path2, "/agency.txt")))
  unlink(path2)
})

test_that("write_gtfs with shapes", {
  skip_on_cran()
  duke_sf = gtfs_as_sf(gtfs_duke)
  tmppath = tempfile(fileext = ".zip")
  write_gtfs(duke_sf, tmppath)
  gtfs_duke_reread = read_gtfs(tmppath)
  
  for(nn in names(gtfs_duke)[which(names(gtfs_duke) != ".")]) {
    expect_equal(gtfs_duke_reread[[nn]][colnames(gtfs_duke[[nn]])], gtfs_duke[[nn]], tolerance = 0.001)
  }
  expect_equal(gtfs_duke_reread$.$dates_services, gtfs_duke$.$dates_services)
})
