context('Converting GTFS routes and shapes into simple feature (sf) dataframes')
library(sf)

# convert_gtfs_routes_to_sf()
test_that('Can convert a routes dataframe to a simple features dataframe', {
	expect_is(routes_df_as_sf(gtfs_obj), 'sf')
})

test_that('Can convert a stops dataframe to a simple features dataframe', {
  expect_is(stops_df_as_sf(gtfs_obj$stops), 'sf')
})