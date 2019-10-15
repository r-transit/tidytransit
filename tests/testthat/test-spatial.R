context("Converting GTFS routes and shapes into sf dataframes")
library(sf)

test_that("Can convert a gtfsr routes dataframe 
          to a simple features dataframe", {
	expect_is(get_route_geometry(gtfs_duke), "sf")
})

test_that("Can convert a gtfsr stops dataframe 
          to a simple features dataframe", {
  expect_is(get_stop_geometry(gtfs_duke$stops), "sf")
})

nyc_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
nyc <- read_gtfs(nyc_path)

test_that("convert shapes to shapes_sf", {
  shapes_sf = get_shapes_geometry(nyc$shapes)
  expect_is(shapes_sf, "sf")
  expect_equal(nrow(shapes_sf), length(unique(nyc$shapes$shape_id)))
})

test_that("gtfs_as_sf doesn't crash without shapes", {
  nyc$shapes <- NULL
  expect_warning(gtfs_as_sf(nyc))
})
