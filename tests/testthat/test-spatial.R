context("Converting to sf objects")
library(sf)

test_that("convert gtfs stops and shapes to sf data frames", {
  expect_is(stops_as_sf(gtfs_duke$stops), "sf")
  shapes_sf = shapes_as_sf(gtfs_duke$shapes)
  expect_is(shapes_sf, "sf")
  expect_equal(nrow(shapes_sf), length(unique(gtfs_duke$shapes$shape_id)))
  duke_sf <- gtfs_as_sf(gtfs_duke)
  expect_is(duke_sf$shapes, "sf")
  expect_is(duke_sf$stops, "sf")
  duke_sf2 = gtfs_as_sf(duke_sf)
  expect_equal(duke_sf2, duke_sf)
})

test_that("gtfs_as_sf doesn't crash without shapes", {
  gtfs_duke_wo_shapes <- gtfs_duke
  gtfs_duke_wo_shapes$shapes <- NULL
  expect_silent(gtfs_as_sf(gtfs_duke_wo_shapes))
  expect_silent(gtfs_as_sf(gtfs_duke_wo_shapes, skip_shapes = TRUE))
  gtfs_duke_wo_shapes$stops <- NULL
  expect_error(gtfs_as_sf(gtfs_duke_wo_shapes), "No stops table in feed")
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

test_that("crs is used", {
  duke_sf = gtfs_as_sf(gtfs_duke)
  expect_equal(st_crs(duke_sf$stops)$input, "EPSG:4326")
  
  duke_sf_crs = gtfs_as_sf(gtfs_duke, crs = 3358)
  expect_equal(st_crs(duke_sf_crs$stops)$input, "EPSG:3358")
  expect_equal(st_crs(duke_sf_crs$shapes)$input, "EPSG:3358")
  duke_sf_crs2 = gtfs_transform(duke_sf, 3358)
  expect_equal(st_crs(duke_sf_crs2$shapes)$input, "EPSG:3358")
  expect_equal(gtfs_transform(gtfs_duke, 3358), duke_sf_crs)
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
  pl1 = plot(gtfs_duke)
  pl2 = plot(duke_sf)
  gtfs_duke_wo_stops <- gtfs_duke
  gtfs_duke_wo_stops$stops <- NULL
  expect_error(plot(gtfs_duke_wo_stops))
})

test_that("meaningful errors", {
  expect_error(get_route_geometry(gtfs_duke), "shapes not converted to sf, use gtfs_obj <- gtfs_as_sf(gtfs_obj)", fixed = TRUE)
  expect_error(get_trip_geometry(gtfs_duke), "shapes not converted to sf, use gtfs_obj <- gtfs_as_sf(gtfs_obj)", fixed = TRUE)
  
  gtfs_as_sf(gtfs_duke, quiet = FALSE)
})

test_that("sf_as_tbl", {
  duke_00 = gtfs_duke
  duke_sf = gtfs_as_sf(duke_00, crs = 3358)
  duke_df = sf_as_tbl(duke_sf)
  attributes(duke_00$shapes)$.internal.selfref <- NULL
  
  expect_equal(duke_df$stops[colnames(gtfs_duke$stops)], gtfs_duke$stops, tolerance = 0.0001)
  
  x = duke_df$shapes[colnames(duke_00$shapes)] %>% arrange(shape_id, shape_pt_sequence)
  y = duke_00$shapes %>% arrange(shape_id, shape_pt_sequence)
  
  expect_equal(x, y, tolerance = 0.001)
})

