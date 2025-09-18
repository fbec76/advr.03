#' Implementation of the Euclidean algorithm
#' to find the greatest common divisor of two numbers
#' Wikipedia: //en.wikipedia.org/wiki/Euclidean algorithm
#'
#' @param x numeric scalar or integer
#' @param y numeric scalar or integer
#' @return greatest common divisor of x and y
#' @export
euclidean <- function(x, y){
  
  stopifnot(is.numeric(x) || is.integer(x))
  stopifnot(is.numeric(y) || is.integer(y))
  
  repeat{
    if(x == y){
      cdg <- x
      break
    }
    if(x > y){
      x <- x-y
    } else {
      y <- y-x
    }
  }
  return(cdg)
}