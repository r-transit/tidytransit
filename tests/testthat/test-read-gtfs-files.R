library(gtfsr)
context('Reading GTFS files/zip directory')

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

get_feed2 <- function(url, path = NULL) {
	get_feed(url, path = path, quiet = TRUE)
}

unzip_gtfs_files2 <- function(zip, delete_zip = FALSE) {
	suppressMessages(suppressWarnings(unzip_gtfs_files(zip, delete_zip = delete_zip)))
}

read_gtfs2 <- function(file_path, delete_files) {
	suppressMessages(suppressWarnings(read_gtfs(file_path, delete_files)))
}

# unzip_gtfs_files()
test_that('Download, extract a GTFS zip file to temp or user-specified path from user-specified URL', {

	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	zip <- get_feed2(url)

	# non-specified path
	expect_true(dir.exists(unzip_gtfs_files2(zip))) # unzips to folder
	expect_warning(unzip_gtfs_files(zip)) # folder already warning

	# specified path
	dir <- tempdir()
	zip <- get_feed2(url, path = dir)
	expect_true(file.exists(zip)) # zip file is found
	expect_true(dir.exists(unzip_gtfs_files2(zip))) # unzips to folder
	expect_warning(unzip_gtfs_files(zip)) # folder already exists warning

	# valid path check
	path <- "#!:D"
	expect_error(get_feed2(url, path = path)) # invalid path

})

# read_gtfs()
test_that('Read GTFS file from unzipped folder', {

	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	zip <- get_feed2(url)
	folder <- unzip_gtfs_files(zip)
	files <- list.files(folder, full.names = TRUE)

	expect_type(read_gtfs2(folder, delete_files = FALSE), 'list')
	expect_is(read_gtfs2(folder, delete_files = FALSE), 'gtfs')

	file.remove(files[1]) # remove agency.txt
	expect_null(read_gtfs2(folder)) # no 'agency.txt' found, returns NULL

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

