context("Import metadata from transitfeeds")

working <- function() {
  url <- "https://github.com/r-transit/tidytransit/raw/master/inst/extdata/sample-feed-fixed.zip"
  connecting <- function(gtfs_url) {
    r <- tryCatch(httr::GET(gtfs_url, httr::timeout(5)), 
                  error = function(e) NA)
    if(!is.na(r[1])) r$status_code == 200 else FALSE
  }
  connecting(url)
}

test_that("the metadata from transitfeeds is a data frame that is not empty", {
  skip_on_cran()
  tfkey <- Sys.getenv("TRANSITFEED_API")
  if(!working()){
    skip("no internet, skipping")
  }
  else if (identical(tfkey, "")) {
    skip("no API key, skipping")
  }
  else {
    feedlist_df <- get_feedlist()
    expect_is(feedlist_df, "data.frame")
    expect_true(dim(feedlist_df)[1]>100)
  }
})
