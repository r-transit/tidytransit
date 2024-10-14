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
#' \url{https://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip}. 
#' @docType data
#' @seealso [read_gtfs()]
"gtfs_duke"
