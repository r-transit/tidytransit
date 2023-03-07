routing.zip = system.file("extdata", "routing.zip", package = "tidytransit")

test_that("as_tidygtfs w/ gtfstools", {
  y = read_gtfs(routing.zip)
  gtfstools_gtfs = readRDS(system.file("extdata", "gtfstools_example.rds", package = "tidytransit"))
  
  x = as_tidygtfs(gtfstools_gtfs)
  expect_equal(names(x), names(y))
  expect_equal(names(x$.), names(y$.))
  
  for(table_name in names(x)) {
    expect_equal(x[[table_name]], y[[table_name]])
  }
})

test_that("as_tidygtfs w/ list", {
  x1 = read_gtfs(routing.zip)
  
  gtfs_list <- lapply(x1, function(y) {
    dplyr::as_tibble(y)
  })
  gtfs_list$. <- NULL
  gtfs_list$stop_times$departure_time <- as.character(gtfs_list$stop_times$departure_time)
  
  expect_equal(class(gtfs_list), "list")
  
  x2 = as_tidygtfs(gtfs_list)
  
  expect_equal(x2, x1)
})
