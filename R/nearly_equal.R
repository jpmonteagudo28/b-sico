#' Compare two numeric vectors for equality.
#' This is a safe way of comparing if two vectors of floating point numbers are (pairwise) equal.  This is safer than
#' using `==`, because it has a built in tolerance.
#'
#' @param x,y Numeric vectors to compare
#' @param tol Tolerance of comparison.
#' @return Logical vector of the same length as x and y.
#' @examples
#' nearly_equal(1:10, 1:10 + 1e-10)
#' nearly_equal(1:10, 1:10 + 1e-10, tol = 1e-11)
#' @export
nearly_equal <- function(x, y, tol = 1e-10){
  abs(x - y) < tol
}
