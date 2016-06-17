library(gtfsr)
context('Importing a GTFS object')

not_working <- function() {
	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	connecting <- function(url) {
		r <- base::try(httr::GET(url, httr::timeout(5)))
		if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
	}
	connecting(url)
}

check_url <- function() {
  if (not_working()) {
    skip("Test URL not available.")
  }
}

#import_gtfs()
test_that('Import a GTFS object from URL', {

	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	not_zip <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zippy"
	bad_url <- "https://developers.google.com/transit/gtfs/examples/sample-feed-bad.zip"
	df <- data.frame(url_d = c(url, not_zip, bad_url), stringsAsFactors = FALSE) %>% dplyr::tbl_df(.)

	# non-specified path
	x <- df %>% import_gtfs(quiet=TRUE) %>% sapply(. %>% is.null %>% magrittr::not(.)) %>% sum
	expect_true(x == 1) # unzips to folder
	expect_is(url %>% import_gtfs(quiet = TRUE), 'gtfs') # should return 'list' object
	expect_warning(not_zip %>% import_gtfs) # not zip file warning

	# valid path check
	path <- "#!:D"
	expect_warning(path %>% import_gtfs) # invalid path

})
