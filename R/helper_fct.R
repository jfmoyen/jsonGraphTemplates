#######################################################
#
#          Helper functions
#
#######################################################

#### is color ? ####
#' Check whether a string (or strings) is a valid color name
#' https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
#'
#' @param x character vector, the strings to test
isColor <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}
