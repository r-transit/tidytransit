library(trread)
context('Reading GTFS files/zip directory')

working <- function() {
	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	connecting <- function(url) {
		r <- base::try(httr::GET(url, httr::timeout(5)))
		if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
	}
	connecting(url)
}

# unzip_gtfs_files()
test_that('Download, extract a GTFS zip file to temp or user-specified path from user-specified URL', {
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {
	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	zip <- download_from_url(url)

	# non-specified path
	expect_true(dir.exists(unzip_file(zip))) # unzips to folder
	expect_warning(unzip_file(zip, quiet=TRUE)) # folder already warning

	# specified path
	dir <- tempdir()
	zip <- download_from_url(url, path = dir)
	expect_true(file.exists(zip)) # zip file is found
	expect_true(dir.exists(unzip_file(zip))) # unzips to folder
	expect_warning(unzip_file(zip, quiet = TRUE)) # folder already exists warning
  }
})

# read_gtfs()
test_that('Reading GTFS files from unzipped folder', {
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {
  
	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	zip <- download_from_url(url)
	folder <- unzip_file(zip)
	files <- list_files(folder)

	expect_is(read_and_validate(files, delete_files = FALSE), 'gtfs')

	# remove required file
	# invisible(file.remove(files[1]))
	# files <- list_files(folder)
	# 
	# print(class(read_and_validate(files, delete_files = FALSE))=="gtfs")

	# valid path check
	path <- "#!:D"
	expect_error(import_gtfs(exdir = path, delete_files = TRUE)) # invalid path
  }
})

# parse_gtfs()
test_that('Check if/how we are parsing files', {
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {
    url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
    zip <- download_from_url(url)
    folder <- unzip_file(zip, delete_zip = TRUE)
    files <- list.files(folder, full.names = TRUE)
    agency_file <- files[1]
    # empty file
    empty <- ''
    write(empty, file.path(folder, '_empty.txt'))
    files <- list.files(folder, full.names = TRUE)
    empty_file <- files[1]
    
    expect_null(trread:::parse_gtfs('_empty', empty_file)) # expect null since file is empty
    expect_is(trread:::parse_gtfs('agency', agency_file), 'tbl_df') # check for tibble object
  }
})

