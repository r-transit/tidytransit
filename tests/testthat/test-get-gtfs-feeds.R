library(gtfsr)
context('Getting a GTFS URL')

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

get_feed2 <- function(url, path = NULL) get_feed(url, path = path, quiet = TRUE)

filter_feedlist2 <- function(df) suppressMessages(filter_feedlist(df))

# get_feed()
test_that('Only one URL imported and is valid', {

	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	not_zip <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zippy"
	bad_url <- "https://developers.google.com/transit/gtfs/examples/sample-feed-bad.zip"
	df <- data.frame(url_d = c(url, bad_url), stringsAsFactors = FALSE) %>% dplyr::tbl_df(.)

	expect_true(file.exists(get_feed2(url))) # zip file is found
	expect_true(file.exists(get_feed2(df[1, ]))) # single row/column element ok (even if class 'data.frame' or 'tbl_df')

	expect_null(suppressWarnings(get_feed2(bad_url))) # urls that don't connect return NULL
	expect_null(suppressWarnings(get_feed2(not_zip))) # urls that don't end in zip return NULL

	#warnings
	expect_warning(get_feed(bad_url))
	expect_warning(get_feed(not_zip))

	expect_error(get_feed2(c(url, bad_url))) # cannot have more than 1 url in vector
	expect_error(get_feed2(cbind(df, df))) # must be a single row or column
	expect_error(get_feed2(df[ ,1])) # cannot have more than 1 url in column

})

# filter_feedlist()
test_that('Proper URL filtering from vector/list/data frame', {

	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	not_zip <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zippy"
	bad_url <- "//developers.google.com/transit/gtfs/examples/sample-feed-bad.php"
	df <- data.frame(url_d = c(url, not_zip, bad_url), stringsAsFactors = FALSE) %>% dplyr::tbl_df(.)

	expect_true(dim(filter_feedlist2(df))[1] == 1) # should keep only the single valid url
	expect_error(filter_feedlist2(url)) # must supply df

})

#TODO: somehow test the API extraction without supplying an API key?

