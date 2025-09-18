#' Implementation of the Euclidean algorithm
#' to find the greatest common divisor of two numbers
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
#'
#' @param x numeric scalar or integer
#' @param y numeric scalar or integer
#' @return greatest common divisor of x and y
#' @export
euclid <- function(x, y) {

  stopifnot(is.numeric(x) || is.integer(x))
  stopifnot(is.numeric(y) || is.integer(y))

  x <- abs(x)
  y <- abs(y)

  repeat {
    if (x == y) {
      gcd <- x
      break
    }
    if (x > y) {
      x <- x - y
    } else {
      y <- y - x
    }
  }
  gcd
}
