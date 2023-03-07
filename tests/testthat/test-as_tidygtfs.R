test_that("as_tidygtfs works w/ gtfstools", {
  routing.zip = system.file("extdata", "routing.zip", package = "tidytransit")
  y = read_gtfs(routing.zip)
  gtfstools_gtfs = readRDS(system.file("extdata", "gtfstools_example.rds", package = "tidytransit"))
  
  x = as_tidygtfs(gtfstools_gtfs)
  expect_equal(names(x), names(y))
  expect_equal(names(x$.), names(y$.))
  
  for(table_name in names(x)) {
    expect_equal(x[[table_name]], y[[table_name]])
  }
})

