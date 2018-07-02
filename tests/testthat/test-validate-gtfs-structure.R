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

test_that('Validate files and variables of a GTFS object', {

	indx <- sapply(urls, working)
	urls <- urls[indx] # keep only working urls
	gtfs_objs <- urls %>% import_gtfs(quiet=TRUE)
	n <- length(urls)

	# check sub functions
	prov_dfs <- lapply(gtfs_objs, validate_files_provided)
	vars_dfs <- mapply(validate_vars_provided, val_files = prov_dfs, gtfs_obj = gtfs_objs, SIMPLIFY = FALSE)

	x <- c("file", "spec", "provided_status")
	expect_identical(sapply(prov_dfs, names), matrix(rep(x, n), length(x), n))

	x <- c("file", "file_spec", "file_provided_status", "field", "field_spec",
				"field_provided_status", "validation_status", "validation_details")
	expect_identical(sapply(vars_dfs, names), matrix(rep(x, n), length(x), n))

	# check required fields conditional on getting url
	gtfs_list <- lapply(gtfs_objs, validate_gtfs_structure, quiet = TRUE)
	val_list <- lapply(gtfs_objs, validate_gtfs_structure, return_gtfs_obj = FALSE, quiet = TRUE)

	expect_true(all(sapply(gtfs_list, class) == rep('gtfs', n)))
	expect_true(all(sapply(val_list, class) == rep('list', n)))


	x <- c("tbl_df", "tbl", "data.frame")
	expect_identical(
		gtfs_list %>%
			sapply( . %>%
				attr(which='validate') %>%
				magrittr::extract2('validate_df') %>%
				class), matrix(x, n, length(x))) # check that validate_df is a data.frame

	x <- matrix('logical', 3, n)
	mat <- gtfs_list %>%
			sapply( . %>%
				attr(which='validate') %>%
				magrittr::extract(1:3) %>%
				sapply(. %>% class))
	dimnames(mat) <- NULL
	expect_identical(mat, x) # check first 3 fields are logical

	x <- c("all_req_files", "all_req_fields_in_req_files", "all_req_fields_in_opt_files",
"validate_df")
	mat <- gtfs_list %>%
			sapply( . %>%
				attr(which='validate') %>%
				magrittr::extract(x) %>%
				names)
	expect_identical(mat, matrix(rep(x, n), length(x), n)) # check that it has required names

})
