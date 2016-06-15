library(gtfsr)
context('Reading GTFS files/zip directory')

not_working <- function() {
	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	connecting <- function(url) {
		r <- base::try(httr::GET(url, httr::timeout(3)))
		if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
	}
	connecting(url)
}

check_url <- function() {
  if (not_working()) {
    skip("Test URL not available.")
  }
}

get_feed2 <- function(url, path = NULL) {
	get_feed(url, path = path, quiet = TRUE)
}

unzip_gtfs_files2 <- function(zip, delete_zip = FALSE, quiet = TRUE) {
	suppressMessages(suppressWarnings(unzip_gtfs_files(zip, delete_zip = delete_zip, quiet = quiet)))
}

read_gtfs2 <- function(file_path, delete_files) {
	suppressMessages(suppressWarnings(read_gtfs(file_path, delete_files, quiet = TRUE)))
}

# unzip_gtfs_files()
test_that('Download, extract a GTFS zip file to temp or user-specified path from user-specified URL', {

	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	zip <- get_feed2(url)

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

# read_gtfs()
test_that('Reading GTFS files from unzipped folder', {

	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	zip <- get_feed2(url)
	folder <- unzip_gtfs_files2(zip)
	files <- list.files(folder, full.names = TRUE)

	expect_is(read_gtfs2(folder, delete_files = FALSE), 'gtfs')

	# valid path check
	path <- "#!:D"
	expect_error(read_gtfs2(exdir = path, delete_files = TRUE)) # invalid path

})

# parse_gtfs()
test_that('Check if/how we are parsing files', {

	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	zip <- get_feed2(url)
	folder <- unzip_gtfs_files2(zip, delete_zip = TRUE)
	files <- list.files(folder, full.names = TRUE)
	agency_file <- files[1]
	# empty file
	empty <- ''
	write(empty, file.path(folder, '_empty.txt'))
	files <- list.files(folder, full.names = TRUE)
	empty_file <- files[1]

	expect_null(parse_gtfs('_empty_df', empty_file)) # expect null since file is empty
	expect_is(parse_gtfs('agency_df', agency_file), 'tbl_df') # check for message

})

