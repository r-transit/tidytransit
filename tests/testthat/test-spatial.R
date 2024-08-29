library(sf)

test_that("convert gtfs stops and shapes to sf data frames", {
  expect_s3_class(stops_as_sf(gtfs_duke$stops), "sf")
  shapes_sf = shapes_as_sf(gtfs_duke$shapes)
  expect_s3_class(shapes_sf, "sf")
  expect_equal(nrow(shapes_sf), length(unique(gtfs_duke$shapes$shape_id)))
  duke_sf <- gtfs_as_sf(gtfs_duke)
  expect_s3_class(duke_sf$shapes, "sf")
  expect_s3_class(duke_sf$stops, "sf")
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

  expect_equal(duke_df$stops[colnames(gtfs_duke$stops)], gtfs_duke$stops, tolerance = 0.0001, check.attributes = FALSE)
  
  x = duke_df$shapes[colnames(duke_00$shapes)] %>% arrange(shape_id, shape_pt_sequence)
  y = duke_00$shapes %>% arrange(shape_id, shape_pt_sequence)
  
  expect_equal(x, y, tolerance = 0.001, check.attributes = FALSE)
})

# stop distances ####
stopdist_df = dplyr::tibble(
  stop_id = c("A1", "A2", "A3", "B1", "B2"), stop_name = c("A", "A", "A", "B", "B"),
  stop_lon = c(8.47157, 8.47202, 8.47084, 8.45870, 8.45940),
  stop_lat = c(47.18196, 47.18243, 47.18262, 47.18030, 47.18081),
  fake_lon = 1:5)

test_that("stop_distances", {
  expect_error(stop_distances(list()))
  dist_df = stop_distances(stopdist_df)
  stopdist_sf = stops_as_sf(stopdist_df)
  dist_sf = stop_distances(stopdist_sf)
  
  expect_equal(colnames(dist_df), c("from_stop_id", "to_stop_id", "distance"))
  expect_equal(dist_df[,c("from_stop_id", "to_stop_id")], dist_sf[,c("from_stop_id", "to_stop_id")])
  diff = dist_df$distance - dist_sf$distance
  expect_lt(max(abs(diff)), 1)
})

test_that("geodist", {
  xlon = c(8.4590, 8.4714)
  xlat = c(47.1812, 47.1824)
  dist1 = geodist_list(xlon, xlat)
  expect_type(dist1, "list")
  expect_equal(length(dist1), 1)
  
  x_sf = sf::st_as_sf(data.frame(lon = xlon, lat = xlat), coords = c("lon", "lat"), crs = 4326)
  dist2 = geodist_list_sf(x_sf)
  diff = dist1[[1]]-dist2[[1]]

  expect_lt(max(abs(diff)), 1.5)
})

test_that("stop_group_distances", {
  x = stop_group_distances(stopdist_df)
  expect_equal(colnames(x), c("stop_name", "distances", "n_stop_ids", "dist_mean", "dist_median", "dist_max"))
  expect_true(is.matrix(x$distances[1][[1]]))
  expect_equal(x$n_stop_ids, c(3,2))
})

test_that("stop_group_distances real feed", {
  skip_on_cran()
  g_nyc = read_gtfs(system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit"))
  
  x1 = stop_group_distances(g_nyc$stops)

  g_nyc_sf = gtfs_as_sf(g_nyc)
  x2 = stop_group_distances(g_nyc_sf$stops, "stop_name")

  expect_equal(colnames(x1), colnames(x2))
  expect_equal(x1$stop_name, x2$stop_name)
  for(col in c("n_stop_ids", "dist_mean", "dist_median", "dist_max")) {
    expect_equal(x1[[col]], x2[[col]])
  }
  expect_error(stop_group_distances(g_nyc_sf, "unknown"), "column unknown does not exist in g_nyc_sf")
  
  x3 = stop_group_distances(g_nyc$stops[c(1,4),], "stop_id")
  expect_equal(nrow(x3), 2)  
})

test_that("stops cluster", {
  skip_on_cran()
  
  g_nyc = read_gtfs(system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit"))
  g_nyc2 <- filter_feed_by_area(g_nyc, c(-74.0144, 40.7402, -73.9581, 40.7696))

  x1 = cluster_stops(g_nyc2$stops)
  expect_true(c("stop_name_cluster") %in% colnames(x1))
  x2 = cluster_stops(g_nyc2$stops, max_dist = 5000, "stop_id", "stop_id_cluster")
  expect_equal(length(unique(x2$stop_id)), length(unique(x2$stop_id_cluster)))
  x3 = cluster_stops(g_nyc2$stops, max_dist = 2000, "stop_name", "stop_name")
  expect_gt(nrow(filter(x3, grepl("\\[1\\]", stop_name))), 0)

  # with sf
  g_nyc_sf <- gtfs_as_sf(g_nyc2)
  x4 = cluster_stops(g_nyc_sf$stops)
  expect_equal(length(unique(x1$stop_name_cluster)), length(unique(x4$stop_name_cluster)))
  
  # piping gtfs_obj
  g_nyc2 = cluster_stops(g_nyc2)
  expect_s3_class(g_nyc2, "tidygtfs")
})

test_that("handle feeds with geojson",{
  locations_path = system.file("extdata", "locations_feed.zip", package = "tidytransit")
  
  # read feed
  g1 = read_gtfs(locations_path)
  
  expect_is(g1[["locations"]], "sf")
  expect_equal(nrow(g1[["locations"]]), 2)
  
  g2 = read_gtfs(locations_path, files = "locations")
  expect_equal(g2$locations, g1$locations)
})
