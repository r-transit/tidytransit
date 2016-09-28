#'	Used to remove directory and its content
#' @param folder Character. Path to folder.
#' @noRd

rmfolder <- function(folder) {
	lapply(list.files(folder, full.names=TRUE), file.remove)
	file.remove(folder)
}

#' Used to check if a url is valid
#' @param url Character. URL.
#' @param timeout Integer. Seconds before timeout.
#' @param quiet Boolean. Whether to display output.
#' @param test_url Boolean. Whether to test if the url connects or not. FALSE by default (can take a while).

valid_url <- function(url, timeout = 5, test_url = TRUE, quiet = TRUE) {

	stopifnot(is.character(url))

	connecting <- function(url) {
		r <- base::try(httr::GET(url, timeout = timeout, silent = TRUE))
		if(!assertthat::is.error(r)) {
			r$status_code == 200
		} else {
			if(!quiet) message("Timeout.")
			return(FALSE)
		}
	}

	url_cond1 <- grepl('http[s]?://.*', url) # valid zip file

	# if valid zip file, test to see if anything connects
	if(test_url) {
		if(url_cond1) url_cond2 <- connecting(url) else url_cond2 <- FALSE
	} else url_cond2 <- NULL

	if(!quiet & test_url) {
		message(sprintf("Validating '%s'...", url))
		if(all(c(url_cond2, url_cond1))) message("PASS") else message("FAIL")
	}

	return(all(url_cond1, url_cond2))

}

#' Used to trigger suppressWarnings or not
#' @param expr expression to suppress
#' @param quiet Logical. Whether to suppress or not. \code{FALSE} by default.

trigger_suppressWarnings <- function(expr, quiet) {

	if(!quiet) expr else suppressWarnings(expr)

}
