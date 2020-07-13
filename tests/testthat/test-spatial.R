context("Converting GTFS routes and shapes into sf dataframes")
library(sf)

test_that("convert gtfs stops and shapes to sf data frames", {
  expect_is(stops_as_sf(gtfs_duke$stops), "sf")
  shapes_sf = shapes_as_sf(gtfs_duke$shapes)
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
  expect_silent(gtfs_as_sf(gtfs_duke_wo_shapes, skip_shapes = TRUE))
})

duke_sf <- gtfs_as_sf(gtfs_duke)
test_that("get_route_geometry", {
  get_route_geometry(duke_sf, route_ids = "1681")
  get_route_geometry(duke_sf, route_ids = "12945", service_ids = c("c_16865_b_19493_d_31", "c_839_b_20026_d_31"))
  get_route_geometry(duke_sf, service_ids = "c_839_b_20026_d_31")
  expect_warning(get_route_geometry(duke_sf, route_ids = "non_existing_id"))
  expect_warning(get_route_geometry(duke_sf, route_ids = "1681", service_ids = "non_existing_id"))
  get_trip_geometry(duke_sf, c("t_94482_b_20026_tn_2", "t_94481_b_20026_tn_7"))
  expect_warning(get_trip_geometry(duke_sf, c("t_94482_b_20026_tn_2", "non_existing_id", "other_id")))
})

test_that("route_geometry behaves as before", {
  route_geom <- get_route_geometry(duke_sf)
  expect_equal(nrow(route_geom), 
               length(unique(duke_sf$routes$route_id)))
  expect_equal(sort(route_geom$route_id), 
               sort(duke_sf$routes$route_id))
  expect_equal(length(unique(as.character(sf::st_geometry_type(route_geom$geometry)))), 
               1)
  expect_equal(as.character(sf::st_geometry_type(route_geom$geometry[1])), 
               "MULTILINESTRING")
})

test_that("one shape per trip is returned", {
  n_ids = 14
  trip_ids = sample(unique(duke_sf$trips$trip_id), n_ids)
  trip_geom = get_trip_geometry(duke_sf, trip_ids)
  expect_equal(nrow(trip_geom), n_ids)
})

test_that("two shapes are returned even if trips use the same shape_id", {
  route_id = "12945"
  trip_ids = c("t_726295_b_19493_tn_37", "t_726295_b_19493_tn_39")
  shape_id = "p_531836"
  
  trip_geom = get_trip_geometry(duke_sf, trip_ids)
  expect_equal(nrow(trip_geom), length(trip_ids))
  route_geom = get_route_geometry(duke_sf, route_ids = route_id)
  expect_equal(nrow(route_geom), length(route_id))
})

test_that("plots work with and without shapes", {
  plot(gtfs_duke)
  plot(duke_sf)
  gtfs_duke_wo_stops <- gtfs_duke
  gtfs_duke_wo_stops$stops <- NULL
  expect_error(plot(gtfs_duke_wo_stops))
})

test_that("meaningful errors", {
  expect_error(get_route_geometry(gtfs_duke), "shapes not converted to sf, use gtfs_obj <- gtfs_as_sf(gtfs_obj)", fixed = TRUE)
  expect_error(get_trip_geometry(gtfs_duke), "shapes not converted to sf, use gtfs_obj <- gtfs_as_sf(gtfs_obj)", fixed = TRUE)
  
  gtfs_as_sf(gtfs_duke, quiet = FALSE)
})
