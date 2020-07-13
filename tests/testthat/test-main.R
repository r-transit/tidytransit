context("Import metadata from transitfeeds")

working <- function() {
  url <- "https://github.com/r-transit/tidytransit/raw/master/inst/extdata/sample-feed-fixed.zip"
  connecting <- function(url) {
    r <- base::try(httr::GET(url, httr::timeout(5)))
    if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
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

test_that("summary.gtfs", {
  gpath <- system.file("extdata", "routing.zip", package = "tidytransit")
  g1 = read_gtfs(gpath)
  x1 = capture.output(summary(g1))
  g2 <- set_date_service_table(g1)
  x2 = capture.output(summary(g2))
  expect_true(all(x1 == x2))
})
