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

test_that('Reading GTFS files from unzipped folder', {
  skip_on_cran()
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
    folder <- unzip_file(zip)
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

