context("Converting GTFS routes and shapes 
        into simple feature (sf) dataframes")
library(sf)

test_that("Can convert a gtfsr routes dataframe 
          to a simple features dataframe", {
	expect_is(get_route_geometry(gtfs_obj), "sf")
})

test_that("Can convert a gtfsr stops dataframe 
          to a simple features dataframe", {
  expect_is(get_stop_geometry(gtfs_obj$stops), "sf")
})