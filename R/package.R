#' A package for plotting GTFS data.
#'
#' This package allows one to quickly map GTFS data. It also provides a quick and simple way to validate the structure of GTFS data.
#' @docType package
#' @name gtfsr-package
#' @aliases gtfsr
#' @author Danton Noriega-Goodwin \email{danton.noriega@@gmail.com},
#' Elaine McVey \email{elaine@transloc.com}
#' @examples
#'
#'  library(dplyr)
#'  url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
#'  gtfs_obj <- url %>% import_gtfs(quiet=TRUE)
#'  route <- "1693"
#'  map_gtfs(gtfs_obj, route)
NULL


#' Example GTFS data
#' 
#' Data obtained from 
#' \url{http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip}. 
#' @name gtfs_obj
#' @docType data
#' @seealso convert_gtfs_routes_to_sf
NULL 