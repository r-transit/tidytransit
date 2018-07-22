library(trread)
context('Import and Validation')

gtfs_example_url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"

working <- function() {
  skip_on_cran()
  connecting <- function(gtfs_example_url) {
    r <- base::try(httr::GET(gtfs_example_url, httr::timeout(5)))
    if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
  }
  connecting(gtfs_example_url)
}

test_that('Downloading a zip file from a gtfs_example_url returns a file', {
  skip_on_cran()
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {  
  zip <- trread:::download_from_url(gtfs_example_url)

  expect_true(file.exists(zip))
  }
})

test_that('Unzip and list GTFS files returns more than 4 files', {
  skip_on_cran()
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {
  
  zip <- trread:::download_from_url(gtfs_example_url)
  folder <- trread:::unzip_file(zip)
  files <- trread:::list_files(folder)

  expect_true(length(files)>4)
  }
})

test_that('Read and validate returns a list of class "gtfs"', {
  skip_on_cran()
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {
  
  zip <- trread:::download_from_url(gtfs_example_url)
  folder <- trread:::unzip_file(zip)
  files <- trread:::list_files(folder)

  expect_is(read_and_validate(files), 'gtfs')
  }
})

test_that('import-bad paths throw good errors', {
  skip_on_cran()
  not_a_url <- "#!:D"
  expect_error(import_gtfs(path)) # invalid path
})

# parse_gtfs()
test_that('import-empty txt files are not imported and non-empty ones are imported', {
  skip_on_cran()
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else {
    zip <- trread:::download_from_url(gtfs_example_url)
    folder <- trread:::unzip_file(zip)
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
    # non-specified path
    x <- import_gtfs(gtfs_example_url, quiet=TRUE)
    expect_is(x, 'gtfs') # should return 'list' object
  }
  
})

#import_gtfs()
test_that('the import_gtfs function fails gracefully on bad urls', {
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
  if(working()){
    gtfs_obj1 <- trread::import_gtfs(gtfs_example_url)
    expect_true(gtfs_obj1$validation$all_req_files)
    
    expect_true(dim(gtfs_obj1$validation$full_column_and_file_validation_df)[1]>0)
    expect_true(dim(gtfs_obj1$validation$full_column_and_file_validation_df)[2]==10)
    
    x <- c("all_req_files", "all_req_fields_in_req_files", "all_req_fields_in_opt_files",
           "full_column_and_file_validation_df")
    
    expect_true(table(x %in% names(gtfs_obj1$validation))[['TRUE']]==4) # check that it has required names
  }
})


