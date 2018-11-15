#' Variance
#'
#' This function returns the population variance of a given vector.
#' @param x a numeric vector.
#' @return the population variance of a given vector
#' @export
#' @examples
#' (x <- rnorm(5))
#' var0(x)

var0 <- function(x){mean((x-mean(x))^2)}
