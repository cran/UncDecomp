#' Range
#'
#' This function returns the difference of maximum and minimum of a given vector.
#' @param x a numeric vector.
#' @return the difference of maximum and minimum of a given vector
#' @export
#' @examples
#' (x <- rnorm(5))
#' drange(x)

drange <- function(x){diff(range(x))}
