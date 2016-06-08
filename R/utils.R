#'	Used to remove directory and its content
#' @param folder Character. Path to folder.

rmfolder <- function(folder) {
	lapply(list.files(folder, full.names=TRUE), file.remove)
	file.remove(folder)
}