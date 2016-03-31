
# Purpose -----------------------------------------------------------------

# Functions to use transitfeeds API to get a GTFS feed - 
#   mostly this means a good way to explore the metadata and get the url(s)
#   of the feeds for download
# These should collectively allow a user to 
# - get a df of locations with descriptions and lat/lng coords
# - get a df of feeds (either by location or complete) with urls
# - download the feed from a url as a zip


# Setup -------------------------------------------------------------------

library(readr)
library(httr)
library(dplyr)

# my_transit_feeds_key <- '3d5d789c-1019-4235-b83f-9797167cbb67'
# 
# warn_for_status(
#   r <- GET('http://api.transitfeeds.com/v1/getFeeds', query = list(key = my_transit_feeds_key))
# )
# 
# content_r <- content(r)
# 
# # ids <- sapply(content_r$results$feeds
              
              

# Best practice approach --------------------------------------------------

# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

#' Check HTTP status; stop if failure
#' 
#' @param req The result of a GET
#' 
#' @export 

tfeeds_check <- function(req) {
  if (req$status_code < 400) return(invisible())
  
  message <- tfeeds_text(req)$message
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

#' Parse content as text
#' 
#' @param req The result of a GET
#' 
#' @return Content parsed as text
#' 
#' @export

tfeeds_text <- function(req) {
  parsed_content <- content(req, as = "text")
  if (identical(parsed_content, "")) stop("No output to parse", call. = FALSE)
  parsed_content
}

#' Store API key in a function

tfeeds_key <- function() {
  '3d5d789c-1019-4235-b83f-9797167cbb67'
}

#' Make sure API key string is not empty
#' 
#' @return logical TRUE if key is not empty
#' 
#' @export

has_key <- function() {
  !identical(tfeeds_key(), "")
}

tfeeds_parse_getfeedlist <- function(req) {
  
  # parse content
  parsed_content <- content(req)
  
  # check that parsing produced output
  if (identical(parsed_content, "")) stop("No output to parse", call. = FALSE)
  
  # check for parsed status OK
  identical(parsed_content$status, 'OK')
  
  # check that feed type is as expected
  identical(parsed_content$results, 'locations')
  
  # extract pieces of the feed data
  feeds_flat <- lapply(parsed_content$results$feeds, function(x) {
   
    id <- x$id
    names(id) <- 'id'
    
    t <- x$t
    names(t) <- 't'
    
    l <- unlist(x$l)
    names_l <- paste0('loc_', names(x$l))
    names(l) <- names_l
    
    u <- unlist(x$u)
    names_u <- paste0('url_', names(x$u))
    if (!is.null(u)) {names(u) <- names_u}
    
    data.frame(t(c(id, t, l, u)), stringsAsFactors = FALSE)

  })
  
  # extract data into a dataframe
  feeds_df <- bind_rows(feeds_flat)
  
  feeds_df
  
}

#' Extract location dataframe from transitfeeds getLocation API
#' 
#' @param req Response to getLocation GET 
#' 
#' @return Dataframe of locations with id, descriptions, and lat/lng
#' 
#' @export

tfeeds_parse_getlocation <- function(req) {
  
  # parse content
  parsed_content <- content(req)
  
  # check that parsing produced output
  if (identical(parsed_content, "")) stop("No output to parse", call. = FALSE)
  
  # check for parsed status OK
  identical(parsed_content$status, 'OK')
  
  # check that feed type is as expected
  identical(parsed_content$results, 'locations')
  
  # extract location data into a dataframe
  loc_df <- bind_rows(parsed_content$results$locations)
  
  loc_df
  
}

#' Set up GET for transitfeeds API
#' 
#' @param path Character containing the name of the API call (ex. getFeeds)
#' @param version Version of API as character that can be appended as part of path, defaults to v1
#' @param key API key, defaults to tfeeds_key()
#' 
#' @return Result of GET
#'
#' @details See http://transitfeeds.com/api/ for available API calls
#'   
#' @export

tfeeds_GET <- function(path, query, ..., version = 'v1/', key = tfeeds_key()) {
  
  if (missing(query)) {
    my_query <- list(key = key)
  } else {
    my_query <- c(query, list(key = key))
  }
  
  req <- GET('http://api.transitfeeds.com/', path = paste0(version, path), query = my_query)

  tfeeds_check(req)
  
  req
}


#' Get all locations available from the transitfeeds API (getLocations)
#' 
#' @return A dataframe of locations with id, descriptions, and lat/lng
#' 
#' @export

get_locations <- function() {
  
  req <- tfeeds_GET("getLocations")
  
  tfeeds_parse_getlocation(req)

}

#' Get list of available feeds from transitfeeds API
#' 
#' @return Result of GET
#' 
#' @export

get_feedlist <- function(location_id) {
  
  #TODO: Generalize query to allow for multiple parameters depending on what's provided
  
  max_limit <- 100 # 100 is the max limit
  
  # Submit the appropriate response based on the arguments
  if (missing(location_id)) {
    req <- tfeeds_GET("getFeeds", query = list(limit = max_limit)) 
  } else {
    req <- tfeeds_GET("getFeeds", query = list(location = location_id, limit = max_limit)) ## add location to query string
  }
  
  # Determine whether the max limit is truncating the desired results
  content_req <- content(req)
  total_results <- content_req$results$total
  
  # If the results are truncated by the max limit, run multiple times
  if (total_results > max_limit) {
  
    response_cycles <- ceiling(total_results / max_limit)
    
    req_df <- data_frame()
    
    for (i in 1:response_cycles) {
      
      # assuming with a location_id the max_limit won't be a problem
      # throw an error if I'm wrong
      if (!missing(location_id)) stop('Location-specific queries with more than 100 responses are unsupported.')
      
      sub_req <- tfeeds_GET("getFeeds", query = list(limit = max_limit, page = i)) 
      
      sub_req_df <- tfeeds_parse_getfeedlist(sub_req)
      
      req_df <- rbind(req_df, sub_req_df)
      
    }
      
  } else {
    
    req_df <- tfeeds_parse_getfeedlist(req)
  
  }

  return(req_df)
  
}

#' Download a zipped GTFS feed file from a url
#' 
#' @param url Character URL of GTFS feed.  
#' @param dir Folder into which to put zipped file
#'
#' @return File path
#'   
#' @export

get_feed <- function(url, dir = 'temp') {
  
  # generate a temporary file path
  temp <- paste0(dir, '/', 'gtfs_zip.zip')

  # get the zipped file containing the GTFS feed data
  GET(url, write_disk(temp, overwrite = TRUE))
  
  # return the temp path - for unzipping
  temp
  
}

#' Filter a feedlist for just downloadable results
#' 
#' @param feedlist_df A dataframe of feed metadata such as output from get_feedlist
#' 
#' @return A dataframe of feed metadata for all feeds in input that are downloadable
#' 
#' @export

filter_feedlist <- function(feedlist_df) {
  
  if (!is.data.frame(feedlist_df)) stop('Invalid feedlist_df input.  Must be a dataframe.')
  if (!('url_d' %in% names(feedlist_df))) stop('No valid URLs found - expected url_d column in feedlist_df.')
  
  print(paste0(sum(is.na(feedlist_df$url_d)), ' feeds did not provide downloadable URLs of ', nrow(feedlist_df), ' feeds provided.'))
 
  feedlist_df <- feedlist_df %>%
    filter(!is.na(url_d))
   
  return(feedlist_df)
  
}
