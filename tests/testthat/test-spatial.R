context("Converting GTFS routes and shapes into sf dataframes")
library(sf)

test_that("convert gtfs stops and shapes to sf data frames", {
  expect_is(get_stops_geometry(gtfs_duke$stops), "sf")
  shapes_sf = get_shapes_geometry(gtfs_duke$shapes)
  expect_is(shapes_sf, "sf")
  expect_equal(nrow(shapes_sf), length(unique(gtfs_duke$shapes$shape_id)))
  duke_sf <- gtfs_as_sf(gtfs_duke)
  expect_is(duke_sf$shapes, "sf")
  expect_is(duke_sf$stops, "sf")
})

test_that("gtfs_as_sf doesn't crash without shapes", {
  gtfs_duke_wo_shapes <- gtfs_duke
  gtfs_duke_wo_shapes$shapes <- NULL
  expect_warning(gtfs_as_sf(gtfs_duke_wo_shapes))
})

test_that("get_route_geometry", {
  duke_sf <- gtfs_as_sf(gtfs_duke)
  
  get_route_geometry(duke_sf, route_ids = "1681")
  get_route_geometry(duke_sf, route_ids = "12945", service_ids = c("c_16865_b_19493_d_31", "c_839_b_20026_d_31"))
  get_route_geometry(duke_sf, service_ids = "c_839_b_20026_d_31")
  expect_warning(get_route_geometry(duke_sf, route_ids = "non_existing_id"))
  expect_warning(get_route_geometry(duke_sf, route_ids = "1681", service_ids = "non_existing_id"))
  get_trip_geometry(duke_sf, c("t_94482_b_20026_tn_2", "t_94481_b_20026_tn_7"))
  expect_warning(get_trip_geometry(duke_sf, c("t_94482_b_20026_tn_2", "non_existing_id", "other_id")))
})

test_that("route_geometry behaves as before", {
  gtfs_obj <- gtfs_as_sf(gtfs_duke)
  route_geom <- get_route_geometry(gtfs_obj)
  expect_equal(nrow(route_geom), 
               length(unique(gtfs_obj$routes$route_id)))
  expect_equal(sort(route_geom$route_id), 
               sort(gtfs_obj$routes$route_id))
  expect_equal(length(unique(as.character(sf::st_geometry_type(route_geom$geometry)))), 
               1)
  expect_equal(as.character(sf::st_geometry_type(route_geom$geometry[1])), 
               "MULTILINESTRING")
})
