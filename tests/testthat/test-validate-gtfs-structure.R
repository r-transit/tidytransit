library(trread)
context('Meta GTFS Validation')

working <- function(url) {
	r <- base::try(httr::GET(url, httr::timeout(5)))
	if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
}

url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
url2 <- "https://sites.google.com/site/gotrianglegtfs/google-transit/data/google_transit.zip"
url3 <- "http://data.trilliumtransit.com/gtfs/westcat-ca-us/westcat-ca-us.zip"
urls <- c(url, url2, url3)

check <- function() {
	if(urls %>% sapply(. %>% working) %>% sum == 0) skip("All test URLs not available.")
}

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

test_that('Some minimal validation is performed and returned', {
  skip_on_cran()
	indx <- sapply(urls, working)
	urls <- urls[indx] # keep only working urls
  sapply(urls,check_metadata)

})
