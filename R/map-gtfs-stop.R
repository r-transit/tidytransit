# MAPPING SIMPLE -----------------------------------------------------------------
# "SIMPLE" here refers to mapping the information of a single stop or route.

# Functions to quickly and easily map
#	- a single stop
# - stops for a route
# - shape of a route

#' map a single stop
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param stop_id Character. A single ID for a stop of interest.
#' @param stop_color Character. An R color or hex value. Expects single value or NULL. Default is NULL. If length(stop_color) > 1, then it reverts to `stop_color <- stop_color[1]`
#'
#' @return Leaflet map object with point plotted at stop lat/long value
#' @export

map_gtfs_stop <- function(gtfs_obj, stop_id, stop_color = NULL) {

	stopifnot(
		class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		length(stop_id) == 1
		)

	if(!is.null(stop_color)) {
		if(length(stop_color) > 1) stop_color <- stop_color[1]
	} else {
		stop_color <- 'red'
	}

	id <- stop_id

	df <- gtfs_obj$stops_df %>%
		dplyr::slice(which(stop_id %in% id))

	if(dim(df)[1] == 0) {
		s <- "Stop '%s' was not found." %>% sprintf(id)
		stop(s)
	}

	stop <- df %>%
		dplyr::select(stop_name, stop_lat, stop_lon) %>%
		dplyr::rename(name = stop_name, lat = stop_lat, lng = stop_lon)

	m <- stop %>%
		leaflet::leaflet() %>%
		leaflet::addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
		leaflet::addCircleMarkers(
      popup = stop$name,
      radius = 10,
      weight = 6,
      stroke = TRUE,
      color = 'black',
      opacity = 1,
      fill = TRUE,
      fillColor = stop_color,
      fillOpacity = 1,
      lat = stop$lat,
      lng = stop$lng)
	m

}
