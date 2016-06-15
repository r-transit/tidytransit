#'	Used to remove directory and its content
#' @param folder Character. Path to folder.

rmfolder <- function(folder) {
	lapply(list.files(folder, full.names=TRUE), file.remove)
	file.remove(folder)
}

#' Used to check if a url is valid
#' @param url Character. URL.

valid_url <- function(url) {
	connecting <- function(url) {
		r <- base::try(httr::GET(url, httr::timeout(3)), silent = TRUE)
		if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
	}

	cond1 <- connecting(url) # can connect
	cond2 <- grepl('\\.zip$', basename(url)) # valid zip file

	return(c(cond1, cond2))

}
