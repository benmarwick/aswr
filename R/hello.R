# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}



#' pull a vector or scalar (vector of length one) from a data frame
#'
#' Sometimes we want to get a single value from a dplyr pipeline so we can use it with inline R code in R markdown. This function is an efficient method of extracting a column from a tibble as a vector. From http://stackoverflow.com/a/24730843/1036500
#'
#' @param x a data frame
#' @param y a column name
#'
#' @return a vector or scalar
#' @export
#'
#' @examples
#'
#' pull(iris, Species)
#'
pull <- function(x,y) {
  x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]
}



