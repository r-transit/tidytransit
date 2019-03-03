#' Get list of all available feeds from transitfeeds API
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom utils "read.csv"
#' @importFrom stats "na.omit"
#' @importFrom stats "setNames"
#' @return a data frame with the gtfs feeds on transitfeeds.
#' @seealso feedlist_df
#'
#' @export
#' @examples \donttest{
#' feedlist_df <- get_feedlist() 
#' }

get_feedlist <- function() {

  max_limit <- 100 # 100 is the max limit

  # get the full feedlist
  req <- tfeeds_get("getFeeds", query = list(limit = max_limit))
  content_req <- httr::content(req)
  total_results <- content_req$results$total

  # If the results are truncated by the max limit, run multiple times
  if (total_results > max_limit) {

    response_cycles <- ceiling(total_results / max_limit)
    req_df <- dplyr::data_frame()

    for (i in 1:response_cycles) {

      sub_req <- tfeeds_get("getFeeds", 
                            query = list(limit = max_limit, page = i))
      sub_req_df <- tfeeds_parse_getfeedlist(sub_req)
      req_df <- rbind(req_df, sub_req_df)

    }

  } else {

    req_df <- tfeeds_parse_getfeedlist(req)

  }

  req_df <- req_df %>%
    dplyr::mutate(loc_lng = as.numeric(loc_lng), loc_lat = as.numeric(loc_lat))

  return(req_df)

}

#' Download a zipped GTFS feed file from a url
#'
#' @param url Character URL of GTFS feed.
#' @param path Character. Folder into which to put zipped file. If NULL, then save a tempfile
#' @param quiet Boolean. Whether to see file download progress. FALSE by default.
#'
#' @return File path
#'
#' @keywords internal

get_feed <- function(url, path=NULL, quiet=FALSE) {

  stopifnot(length(url) == 1)

  # check if single element of dataframe 
  # was inputed. if so, convert to single value; error otherwise.
  if(!is.null(dim(url))) {
    if(all(dim(url) == c(1,1))) {
      url <- unlist(url, use.names = FALSE)
    } else {
      stop('Please input a single url.')
    }
  }

  # check if url links to a zip file
  valid <- valid_url(url)
  if(!valid) {
    if(!quiet) {
      warn1 <- 
        sprintf("Link '%s' is invalid; 
                failed to connect. NULL was returned.", url)
      warning(warn1)
    }
    return(NULL)
  }

  # generate a temporary file path if no path is specified
  if(is.null(path)) temp <- 
    tempfile(fileext = ".zip") else 
      temp <- file.path(path, 'gtfs_zip.zip')

  r <- httr::GET(url)

  # Get gtfs zip if url can be reach
  if(httr::status_code(r) == 200) {
    check <- try(utils::download.file(url, temp, quiet = quiet), silent=TRUE)
    if(check %>% assertthat::is.error()) {
      warn <- sprintf("Link '%s' failed to download. NULL was returned.", url)
      warning(warn)
      return(NULL)
    }
  } else {
    warn <- sprintf("Link '%s' cannot be reached. NULL was returned.", url)
    warning(warn)
    return(NULL)
  }

  # return the temp path - for unzipping
  return(temp)

}

#' Filter a feedlist to include only valid urls (ending in .zip and connect)
#'
#' @param feedlist_df A dataframe of feed metadata such as output from get_feedlist
#' @param test_url Boolean. Whether to test if the url connects or not. FALSE by default (can take a while).
#'
#' @return A dataframe of feed metadata 
#' for all feeds in input that are downloadable
#'
#' @keywords internal

filter_feedlist <- function(feedlist_df, test_url = FALSE) {

  if (!is.data.frame(feedlist_df)) 
    stop('Invalid feedlist_df input.  Must be a dataframe.')
  if (!('url_d' %in% names(feedlist_df))) 
    stop('No valid URLs found - expected url_d column in feedlist_df.')

  indx <- feedlist_df$url_d %>%
    sapply(. %>% valid_url(test_url = test_url) %>% all, USE.NAMES = FALSE)
  message(
    paste0(
      sum(!indx),
      ' of ',
      nrow(feedlist_df),
      " feeds did not provide valid URLs. ",
      sum(indx),
      " returned."
    )
  )

  feedlist_df <- feedlist_df %>%
    dplyr::slice(which(indx))

  return(feedlist_df)

}


# API Keys ------------------------------------------------

#' Tools to get, set, or check for API key
#' @noRd
tools_api_key <- function() {

  get <- function() {
    env <- Sys.getenv("TRANSITFEED_API")

    if(identical(env, "")) {
      message("Couldn't find env var TRANSITFEED_API. Please set you api key using the function set_api_key().")
      return(NULL)
    } else env

  }

  set <- function() {
    # have user input api key
    message("Please enter your API key you requested from https://transitfeeds.com/api/keys, and press enter:")
    key <- readline(": ")

    stopifnot(nchar(key) == 36, is.character(key))
    valid_api_key <- grepl("[[:alnum:]]{8}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{12}", key)
    if(!valid_api_key) {
      stop(sprintf("API key '%s' is invalid. API keys are 36 characters long with pattern XXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX.", key))
    }

    text <- paste0("TRANSITFEED_API=",key,"\n")

    env <- Sys.getenv("TRANSITFEED_API")

    # check for existing TRANSITFEED_API
    if (!identical(env, "")) { # if found, replace line and rewrite
      renv <- readLines(file.path(normalizePath("~/"), ".Renviron"))
      loc <- grep("TRANSITFEED_API", renv)
      renv[loc] <- text
      Sys.setenv(TRANSITFEED_API = key)
      writeLines(renv, file.path(normalizePath("~/"), ".Renviron"))
    } else { # if not found, append to file
      Sys.setenv(TRANSITFEED_API = key)
      cat(text, file=file.path(normalizePath("~/"), ".Renviron"), append=TRUE)
    }

  }

  has = function() {
    env <- Sys.getenv("TRANSITFEED_API")
    if(!identical(env, "")) TRUE else FALSE
  }

  clear = function() {
    env <- Sys.getenv("TRANSITFEED_API")

    # clear TRANSITFEED_API variable
    if (!identical(env, "")) { # if found, replace line and rewrite
      renv <- readLines(file.path(normalizePath("~/"), ".Renviron"))
      indx <- grepl("TRANSITFEED_API", renv)
      Sys.setenv(TRANSITFEED_API = "")
      writeLines(renv[!indx], file.path(normalizePath("~/"), ".Renviron"))
    }
  }

  list(get = get, set = set, has = has, clear = clear)

}

#' Assign the tools function environment
#' @noRd
gtfs_api_key <- tools_api_key()

#' Clear the API key.
#' @keywords internal
clear_api_key <- gtfs_api_key$clear

#' Set TransitFeeds API key for recall
#' @export

set_api_key <- gtfs_api_key$set

api_check <- function() {
  if(!gtfs_api_key$has())
      stop("API key not found. Please obtain a key from transitfeeds.com, ",
           "and set using function 'set_api_key()'")
}

#' Get API key
#' @keywords internal

get_api_key <- function() {
  api_check()
  gtfs_api_key$get()

}


#' Make sure API key string is not empty
#'
#' @return logical TRUE if key is not empty
#'
#' @keywords internal

has_api_key <- function() {
  api_check()
  api_key <- gtfs_api_key$get()

  valid_api_key <- grepl("[[:alnum:]]{8}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{12}", api_key)
  if(!valid_api_key) {
    stop(sprintf("API key '%s' is invalid. API keys for transitfeeds.com are 36 characters long with pattern XXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX. Please set your API key using function 'set_api_key()'", api_key))
  } else valid_api_key
}


# Best practice approach --------------------------------------------------

# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

#' Check HTTP status; stop if failure
#' @param req The result of an httr::GET
#' @noRd

tfeeds_check <- function(req) {
  if (req$status_code < 400) return(invisible())

  message <- tfeeds_text(req)$message
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

#' Parse content as text
#'
#' @param req The result of a httr::GET
#' @noRd
#' @return content parsed as text


tfeeds_text <- function(req) {
  parsed_content <- httr::content(req, as = "text")
  if (identical(parsed_content, "")) stop("No output to parse", call. = FALSE)
  parsed_content
}

#' Parse a gtfs feed list
#'
#' @param req The result of a GET
#' @noRd
#' @return Dataframe of feeds

tfeeds_parse_getfeedlist <- function(req) {

  # parse content
  parsed_content <- httr::content(req)

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
  feeds_df <-dplyr::bind_rows(feeds_flat)

  feeds_df
}

#' Extract location dataframe from transitfeeds getLocation API
#'
#' @param req Response to getLocation httr::GET
#' @noRd
#' @return Dataframe of locations with id, descriptions, and lat/lng
#'

tfeeds_parse_getlocation <- function(req) {

  # parse content
  parsed_content <- httr::content(req)

  # check that parsing produced output
  if (identical(parsed_content, "")) stop("No output to parse", call. = FALSE)

  # check for parsed status OK
  identical(parsed_content$status, 'OK')

  # check that feed type is as expected
  identical(parsed_content$results, 'locations')

  # extract location data into a dataframe
  loc_df <- dplyr::bind_rows(parsed_content$results$locations)

  loc_df

}

#' Set up GET for transitfeeds API
#'
#' @param path Character. Contains the name of the API call (ex. getFeeds)
#' @param query Character. Other query strings.
#' @param version Version of API as character that can be appended as part of path, defaults to v1
#' @param key API key, defaults to gtfs_api_key
#' @param ... any httr::GET options to pass-through
#' @noRd
#' @return Result of httr::GET
#'
#' @details See http://transitfeeds.com/api/ for available API calls
#'

tfeeds_get <- function(path, query, ..., version = 'v1/', key = if(has_api_key()) gtfs_api_key$get()) {

  if (missing(query)) {
    my_query <- list(key = key)
  } else {
    my_query <- c(query, list(key = key))
  }

  req <- httr::GET('http://api.transitfeeds.com/', 
                   path = paste0(version, path), 
                   query = my_query)

  tfeeds_check(req)

  req
}
