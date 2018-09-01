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
"feedlist_df"

#' Example GTFS data
#' 
#' Data obtained from 
#' \url{http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip}. 
#' @name gtfs_obj
#' @docType data
#' @seealso convert_gtfs_routes_to_sf
NULL 
