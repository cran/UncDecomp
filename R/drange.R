#' Range(DEPRECATED)
#'
#' This function returns the difference of maximum and minimum of a given vector.
#' @param x a numeric vector.
#' @return the difference of maximum and minimum of a given vector
#' @export
#' @examples
#' (x <- rnorm(5))
#' # drange() is deprecated. Use U_range()
#' # drange(x)

drange <- function(x){
  .Deprecated(old = "drange", new = "U_range")
  diff(range(x))
}
