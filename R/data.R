#' Dataframe of source GTFS data from Transitfeeds
#'
#' A dataset containing a list of URLs for GTFS feeds
#'
#' @format A data frame with 911 rows and 10 variables:
#' \describe{
#'   \item{id}{the id of the feed on transitfeeds.com}
#'   \item{t}{title of the feed}
#'   \item{loc_id}{location id}
#'   \item{loc_pid}{location placeid of the feed on transitfeeds.com}
#'   \item{loc_t}{the title of the location}
#'   \item{loc_n}{the shortname fo the location}
#'   \item{loc_lat}{the location latitude}
#'   \item{loc_lng}{the location longitude}
#'   \item{url_d}{GTFS feed url}
#'   \item{url_i}{the metadata url for the feed}
#' }
#' @source \url{http://www.transitfeeds.com/}
"feedlist"

#' Dataframe of route type id's and the names of the types (e.g. "Bus")
#' 
#' Extended GTFS Route Types: https://developers.google.com/transit/gtfs/reference/extended-route-types
#' 
#' @docType data
#' @format A data frame with 136 rows and 2 variables:
#' \describe{
#'   \item{route_type}{the id of route type}
#'   \item{route_type_name}{name of the gtfs route type}
#' }
#' @source \url{https://gist.github.com/derhuerst/b0243339e22c310bee2386388151e11e}
"route_type_names"

#' Example GTFS data
#' 
#' Data obtained from 
#' \url{http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip}. 
#' @docType data
#' @seealso read_gtfs
"gtfs_duke"