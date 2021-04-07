context("Service")

gpath <- system.file("extdata", "routing.zip", package = "tidytransit")

test_that("set_servicepattern", {
  gtfs_orig <- read_gtfs(gpath)
  # Create unique service_id for each trip_id
  g = gtfs_orig
  g$trips$service_id <- paste(g$trips$service_id, substr(g$trips$route_id, 5, 6), sep = "_")
  g$calendar <- dplyr::bind_rows(
    g$calendar,
    dplyr::bind_cols(service_id = unique(g$trips$service_id), g$calendar[1,-1])
  )
  g <- set_servicepattern(g)
  expect_equal(length(unique(g$.$servicepatterns$servicepattern_id)), length(unique(gtfs_orig$calendar$service_id)))
})

test_that("set_servicepattern w/ more params", {
  gtfs_orig <- read_gtfs(gpath)
  gtfs_orig <- set_servicepattern(gtfs_orig, hash_algo = "sha256", id_prefix = "pref_", hash_length = -1)

  expect_equal(unique(substr(gtfs_orig$.$servicepatterns$servicepattern_id,0,5)), "pref_")
  expect_error(set_servicepattern(gtfs_orig, hash_algo = "dummy"))
})
