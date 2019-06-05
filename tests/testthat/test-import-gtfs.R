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
  gtfs_obj <- tidytransit:::read_gtfs(
    local_gtfs_path,
    local=TRUE)
  
  expect_is(gtfs_obj, "gtfs")
  file.exists(local_gtfs_path)
})

test_that("Downloading a zip file from a gtfs_example_url returns a file", {
  skip_on_cran()
  if(!working()){
    skip("no internet, skipping")
  }
  else {  
  zip <- tidytransit:::download_from_url(gtfs_example_url, quiet = T)

  expect_true(file.exists(zip))
  }
})

test_that("import-bad paths throw good errors", {
  skip_on_cran()
  not_a_url <- "#!:D"
  expect_error(read_gtfs(path)) # invalid path
})

test_that("import-empty txt files are not 
          imported and non-empty ones are imported", {
  skip_on_cran()
  if(!working()){
    skip("no internet, skipping")
  }
  else {
    zip <- tidytransit:::download_from_url(gtfs_example_url)
    folder <- tidytransit:::unzip_file(zip)
    files <- list.files(folder, full.names = TRUE)
    agency_file <- files[1]
    # empty file
    empty <- ""
    write(empty, file.path(folder, "_empty.txt"))
    files <- list.files(folder, full.names = TRUE)
    empty_file <- files[1]
    
    expect_null(
      tidytransit:::parse_gtfs_file("_empty", 
                                    empty_file))
    expect_is(
      tidytransit:::parse_gtfs_file("agency", 
                                    agency_file), "tbl_df") 
  }
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
    expect_error(tidytransit::read_gtfs(not_zip, quiet=TRUE))
    expect_error(tidytransit::read_gtfs(bad_url, quiet=TRUE)) # not zip file warning
  }
  
})

test_that("Some minimal validation is performed and returned", {
  skip_on_cran()
  if(working()){
    gtfs_obj1 <- tidytransit::read_gtfs(gtfs_example_url)
    
    expect_true(dim(attributes(gtfs_obj1)$validation_result)[1]>0)
    expect_true(dim(attributes(gtfs_obj1)$validation_result)[2]>0)
  }
})

test_that("unknown local file throws meaningful error", {
  tidytransit::read_gtfs(local_gtfs_path)
  expect_error(tidytransit::read_gtfs("/Users/wrong.zip"))
})

test_that("Files with BOM can be read", {
  skip_on_cran()
  bom_path <- system.file("extdata", 
              "sample-feed-bom.zip", 
              package = "tidytransit")
  g <- tidytransit::read_gtfs(bom_path)
  expect_true(is_gtfs_obj(g))
})


