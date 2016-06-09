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
		r <- httr::GET(url)
		r$status_code == 200
	}

	cond1 <- connecting(url) # can connect
	cond2 <- grepl('^http.*zip$', url, ignore.case = TRUE) # full url
	cond3 <- grepl('\\.zip$', basename(url)) # valid zip file

	if(all(cond1, cond2, cond3)) TRUE else FALSE

}
