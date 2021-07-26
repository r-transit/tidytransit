#' Get list of all available feeds from transitfeeds API
#' @importFrom httr content RETRY
#' @importFrom utils "read.csv"
#' @importFrom stats "na.omit"
#' @importFrom stats "setNames"
#' @return a data frame with the gtfs feeds on transitfeeds
#' @seealso feedlist_df
#'
#' @export
#' @examples \dontrun{
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
#' @return No value returned, function is used for setting environment variables
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
#' @param req The result of an httr::RETRY
#' @noRd
tfeeds_check <- function(req) {
  if (req$status_code < 400) return(invisible())

  message <- tfeeds_text(req)$message
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

#' Parse content as text
#'
#' @param req The result of a httr::RETRY
#' @importFrom httr content
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
#' @importFrom httr content
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
#' @param req Response to getLocation httr::RETRY
#' @importFrom httr content
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
#' @param ... any httr::RETRY options to pass-through
#' @importFrom httr RETRY
#' @noRd
#' @return Result of httr::RETRY
#'
#' @details See http://transitfeeds.com/api/ for available API calls
tfeeds_get <- function(path,
                       query,
                       ...,
                       version = 'v1/',
                       key = if(has_api_key()) gtfs_api_key$get()) {

  if (missing(query)) {
    my_query <- list(key = key)
  } else {
    my_query <- c(query, list(key = key))
  }

  req <- httr::RETRY(
    verb = "GET"
    , url = 'http://api.transitfeeds.com/'
    , path = paste0(version, path)
    , query = my_query
    , times = 5
    , terminate_on = c(403, 404)
    , terminate_on_success = TRUE
  )

  tfeeds_check(req)

  req
}
