library(gtfsr)
context('Mapping GTFS objects')

not_working <- function() {
	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	r <- httr::GET(url)
	r$status_code != 200
}

check_url <- function() {
  if (not_working()) {
    skip("Test URL not available.")
  }
}

# gtfs_map_*()
test_that('Mapping single stops and routes', {

	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	zip <- get_feed(url, quiet=TRUE)

	# non-specified path
	expect_true(dir.exists(unzip_gtfs_files2(zip))) # unzips to folder
	expect_warning(unzip_gtfs_files(zip, quiet=TRUE)) # folder already warning

	# specified path
	dir <- tempdir()
	zip <- get_feed2(url, path = dir)
	expect_true(file.exists(zip)) # zip file is found
	expect_true(dir.exists(unzip_gtfs_files2(zip))) # unzips to folder
	expect_warning(unzip_gtfs_files(zip, quiet = TRUE)) # folder already exists warning

})
