# PURPOSE ------------------------------------------------

# - Identify problems that can be found in agency.txt files
# - these "problems" are inspired (stolen) from those in the transitfeed python program by Google

#' Define new_problems environment, which contains functions used in validating GTFS data.

new_problems <- function() {

	unrecognized_column = function() {

	}

	deprecated_column = function() {

	}

	duplicate_column = function() {

	}

	missing_value = function() {

	}

	invalid_value = function() {

	}

	invalid_float_value = function() {

	}

	invalid_non_negative_integer_value = function() {

	}

	duplicate_id = function() {

	}

	invalid_agency_name = function() {

	}

	unused_stop = function() {

	}

	used_station = function() {

	}

	stop_too_far_from_parent_station = function() {

	}

	stops_too_close = function() {

	}

	stations_too_close = function() {

	}

	different_station_too_close = function() {

	}

	stop_too_far_from_shape_with_dist_traveled = function() {

	}

	expiration_date = function() {

	}

	future_service = function() {

	}

	date_outside_valid_range = function() {

	}

	no_service_exceptions = function() {

	}

	too_fast_travel = function() {

	}

	stop_with_multiple_route_types = function() {

	}

	duplicate_trip = function() {

	}

	overlapping_trips_in_same_block = function() {

	}

	transfer_distance_too_big = function() {

	}

	transfer_walking_speed_too_fast = function() {

	}

	other_problem = function() {

	}

	too_many_days_without_service = function() {

	}

	minimum_transfer_time_set_with_invalid_transfer_type = function() {

	}

	too_many_consecutive_stop_times_with_same_time = function() {

	}

	list(
				unrecognized_column = unrecognized_column,
				deprecated_column = deprecated_column,
				duplicate_column = duplicate_column,
				missing_value = missing_value,
				invalid_value = invalid_value,
				invalid_float_value = invalid_float_value,
				invalid_non_negative_integer_value = invalid_non_negative_integer_value,
				duplicate_id = duplicate_id,
				invalid_agency_name = invalid_agency_name,
				unused_stop = unused_stop,
				used_station = used_station,
				stop_too_far_from_parent_station = stop_too_far_from_parent_station,
				stops_too_close = stops_too_close,
				stations_too_close = stations_too_close,
				different_station_too_close = different_station_too_close,
				stop_too_far_from_shape_with_dist_traveled = stop_too_far_from_shape_with_dist_traveled,
				expiration_date = expiration_date,
				future_service = future_service,
				date_outside_valid_range = date_outside_valid_range,
				no_service_exceptions = no_service_exceptions,
				too_fast_travel = too_fast_travel,
				stop_with_multiple_route_types = stop_with_multiple_route_types,
				duplicate_trip = duplicate_trip,
				overlapping_trips_in_same_block = overlapping_trips_in_same_block,
				transfer_distance_too_big = transfer_distance_too_big,
				transfer_walking_speed_too_fast = transfer_walking_speed_too_fast,
				other_problem = other_problem,
				too_many_days_without_service = too_many_days_without_service,
				minimum_transfer_time_set_with_invalid_transfer_type = minimum_transfer_time_set_with_invalid_transfer_type,
				too_many_consecutive_stop_times_with_same_time = too_many_consecutive_stop_times_with_same_time
			)
}


#' Assign function environment to 'problems'
problems <- new_problems()