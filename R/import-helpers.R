#' Checks UTF-8-BOM encoding. Special thanks to @patperu for finding the issue and to @hrbrmstr for the code to help deal with the issue.
#' @param path the path the the text file
#' @param encoding can be one of \code{UTF-8}, \code{UTF-16} or \code{UTF-16BE}.
#'        Although a BOM could be used with UTF-32 and other encodings, such
#'        encodings are rarely used for data transmission and the three supported
#'        encodings are the most likely ones folks in R will be working with from
#'        web APIs.\cr\cr
#'        This function defaults to looking for \code{UTF-8} BOM, but you can
#'        override it.
#' @return \code{TRUE} if response contains a BOM, \code{NA} if an unsupported encoding
#'         was passed (along with a message)
#' @references \href{http://www.unicode.org/faq/utf_bom.html}{UTF-8, UTF-16, UTF-32 & BOM}
#' @noRd
#' @author @@hrbrmstr


has_bom <- function(path, encoding="UTF-8") {

  B <- readBin(path, "raw", 4, 1)
  switch(encoding,
       `UTF-8`=B[1]==as.raw(0xef) & B[2]==as.raw(0xbb) & B[3]==as.raw(0xbf),
       `UTF-16`=B[1]==as.raw(0xff) & B[2]==as.raw(0xfe),
       `UTF-16BE`=B[1]==as.raw(0xfe) & B[2]==as.raw(0xff),
       { message("Unsupported encoding") ; return(NA) }
  )
}