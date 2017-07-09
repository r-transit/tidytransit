# gtfsr 1.0.3 (2016-07-09)

## Major Changes

- Added conversion function to translate `gtfs_obj` routes to `sf`. New function is called `convert_gtfs_routes_to_sf` (#24)
- All plots now have an overlay feature. The user can enable/disable route/stop layers. (#33)

I did a very poor job logging changes for all the prior releases. My bad.

# gtfsr 0.1.0 (2016-06-23)

- Release of Minimum Viable Product (MVP)
	+ Importing from API or any link
	+ Validating GTFS feed file structure
	+ Basic plotting of stops/routes