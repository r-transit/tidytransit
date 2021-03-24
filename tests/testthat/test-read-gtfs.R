context("Import and Validation")

gtfs_example_url <- 
  "https://github.com/r-transit/tidytransit/raw/master/inst/extdata/sample-feed-fixed.zip"
local_gtfs_path <- system.file("extdata", 
                               "google_transit_nyc_subway.zip", 
                               package = "tidytransit")


working <- function() {
  connecting <- function(gtfs_example_url) {
    r <- base::try(httr::GET(gtfs_example_url, httr::timeout(5)))
    if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
  }
  connecting(gtfs_example_url)
}

test_that("read_gtfs() imports a local file to a 
          list of dataframes and doesnt 
          delete the source file", {
  gtfs_obj <- read_gtfs(local_gtfs_path)
  
  expect_is(gtfs_obj, "gtfs")
  file.exists(local_gtfs_path)
})

test_that("loud read_gtfs", {
  expect_is(
    read_gtfs(local_gtfs_path, quiet = FALSE),
    "gtfs")
})

test_that("import-bad paths throw good errors", {
  skip_on_cran()
  not_a_url <- "#!:D"
  expect_error(read_gtfs(path)) # invalid path
})

test_that("the read_gtfs function works", {
  skip_on_cran()
  if(!working()){
      skip("no internet, skipping")
  }
  else {
    # non-specified path
    x <- read_gtfs(gtfs_example_url, quiet=TRUE)
    expect_is(x, "gtfs") # should return 'list' object
  }
  
})

test_that("the read_gtfs function fails gracefully on bad urls", {
  skip_on_cran()
  if(!working()){
      skip("no internet, skipping")
  }
  else {
    not_zip <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zippy"
    bad_url <- "https://developers.google.com/transit/gtfs/examples/sample-feed-bad.zip"
  
    # non-specified path
    expect_error(read_gtfs(not_zip, quiet=TRUE))
  }
  
})

test_that("unknown local file throws meaningful error", {
  read_gtfs(local_gtfs_path)
  expect_error(read_gtfs("/Users/wrong.zip"))
})

test_that("Files with BOM can be read", {
  skip_on_cran()
  bom_path <- system.file("extdata", 
              "sample-feed-bom.zip", 
              package = "tidytransit")
  g <- read_gtfs(bom_path)
  expect_true(is_gtfs_obj(g))
})

test_that("Feed with additional data can be read", {
  g_plus_path <- system.file("extdata", "sample-feed-plus.zip", package = "tidytransit")
  expect_warning(read_gtfs(g_plus_path), "gtfsio/empty_file.txt' has size 0. Returning a NULL data.table.")
  expect_warning(read_gtfs(g_plus_path), "No valid dates defined in feed")
})

test_that("validation", {
  g_invalid_path = system.file("extdata","sample-feed-invalid.zip", package = "tidytransit")
  expect_warning(read_gtfs(g_invalid_path), "Invalid feed. Missing required file(s): stop_times", fixed = TRUE)
  expect_warning(read_gtfs(g_invalid_path), "Invalid feed. Missing required field(s) in stops: stop_id", fixed = TRUE)
})
