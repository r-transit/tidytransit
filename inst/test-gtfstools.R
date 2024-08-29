routing.zip = system.file("extdata", "routing.zip", package = "tidytransit")
gtfstools_gtfs = gtfstools::read_gtfs(routing.zip)

# gtfstools version 1.2.0
saveRDS(gtfstools_gtfs, "inst/extdata/gtfstools_example.rds", compress = "xz")
