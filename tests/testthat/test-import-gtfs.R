library(trread)
context('Reading GTFS files/zip directory')

url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"

working <- function() {
  url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
  connecting <- function(url) {
    r <- base::try(httr::GET(url, httr::timeout(5)))
    if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
  }
  connecting(url)
}

test_that('Downloading from url returns a file that exists', {
  skip_on_cran()
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {  
  zip <- trread::download_from_url(url)

  expect_true(
    file.exists(zip)
    )
  }
})

test_that('Unzip and list GTFS files returns more than 4 files', {
  skip_on_cran()
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {
  
  zip <- trread::download_from_url(url)
  folder <- trread::unzip_file(zip)
  files <- trread::list_files(folder)

  expect_true(length(files)>4)
  }
})

test_that('Read and validate returns a list of class "gtfs" with a non-empty agency_df', {
  skip_on_cran()
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {
  
  zip <- download_from_url(url)
  folder <- unzip_file(zip)
  files <- list_files(folder)

  expect_is(read_and_validate(files), 'gtfs')
  expect_true(dim(files$agency_df)[1]>0)
  }
})

test_that('import-bad paths throw good errors', {
  path <- "#!:D"
  expect_error(import_gtfs(path)) # invalid path
})

# parse_gtfs()
test_that('import-empty txt files are not imported and non-empty ones are imported', {
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {
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

#import_gtfs()
test_that('the import_gtfs function works', {
  skip_on_cran()
  if(working()==FALSE){
      skip("no internet, skipping")
  }
  else {
    url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
  
    # non-specified path
    x <- import_gtfs(url, quiet=TRUE)
    expect_is(x, 'gtfs') # should return 'list' object
  }
  
})

#import_gtfs()
test_that('the import_gtfs function fails gracefully', {
  skip_on_cran()
  if(working()==FALSE){
      skip("no internet, skipping")
  }
  else {
    not_zip <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zippy"
    bad_url <- "https://developers.google.com/transit/gtfs/examples/sample-feed-bad.zip"
  
    # non-specified path
    expect_error(import_gtfs(not_zip, quiet=TRUE))
    expect_error(import_gtfs(bad_url, quiet=TRUE)) # not zip file warning
  }
  
})

test_that('Some minimal validation is performed and returned', {
  skip_on_cran()
  indx <- sapply(urls, working)
  urls <- urls[indx] # keep only working urls
  sapply(urls,check_metadata)
})

check_metadata <- function(url) {
  skip_on_cran()
  gtfs_obj1 <- import_gtfs(url)
  expect_true(gtfs_obj1$validation$all_req_files)
  
  expect_true(dim(gtfs_obj1$validation$full_column_and_file_validation_df)[1]>0)
  expect_true(dim(gtfs_obj1$validation$full_column_and_file_validation_df)[2]==10)
  
  x <- c("all_req_files", "all_req_fields_in_req_files", "all_req_fields_in_opt_files",
         "full_column_and_file_validation_df")
  
  expect_true(table(x %in% names(gtfs_obj1$validation))[['TRUE']]==4) # check that it has required names
}



