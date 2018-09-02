context('Import metadata from transitfeeds')

working <- function() {
  url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
  connecting <- function(url) {
    r <- base::try(httr::GET(url, httr::timeout(5)))
    if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
  }
  connecting(url)
}

#read_gtfs()
test_that('the metadata from transitfeeds is a data frame that is not empty', {
  skip_on_cran()
  tfkey <- Sys.getenv('TRANSITFEED_API')
  if(working()==FALSE){
    skip("no internet, skipping")
  }
  else if (identical(tfkey, "")) {
    skip("no API key, skipping")
  }
  else {
    feedlist_df <- get_feedlist()
    expect_is(feedlist_df, 'data.frame')
    expect_true(dim(feedlist_df)[1]>100)
  }
})
