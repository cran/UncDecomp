##' Slight modifications of apply() and sweep()
##'
##' Slightly modified version of apply() and sweep(). apply0() and sweep0() are modification of apply() and sweep() so that they can be used when the length(MARGIN) is zero. 
##' msweep() is a modification of sweep() so that it can be used when function receives multiple summary statistic.
##'
##' @name apply_sweep
##' @rdname apply_sweep
##'
##' @param X an array.
##' @param MARGIN apply0() : a vector giving the subscripts which the function will be applied over. sweep0(), msweep() : a vector of indices giving the extent(s) of x which correspond to STATS.
##' @param STATS the summary statistic array which is to be swept out. For msweep(), list of summary statistic array.
##' @param FUN the function to be applied. For msweep(), a function that receives the elements of X and list in order
##' @param ... further arguments passed to or from other methods.
##' 
##' @return If each call to FUN returns a vector of length n, then apply() returns an array of dimension c(n, dim(X)[MARGIN]) if n > 1. If n equals 1, apply() returns a scalar if MARGIN has length 0, a vector if MARGIN has length 1 and an array of dimension dim(X)[MARGIN] otherwise.
##' sweep0() and msweep() return an array with the same shape as x, but with the summary statistics swept out.
NULL

##' @rdname apply_sweep
##' @export
apply0<- function(X, MARGIN, FUN, ...){
  if(length(MARGIN)==0)
    return(FUN(c(X)))
  return(apply(X, MARGIN, FUN, ...))
}

##' @rdname apply_sweep
##' @examples
##' set.seed(0)
##' A <- array(rnorm(24), dim = 4:2)
##' meanA0 <- apply0(A, numeric(0), mean)
##' meanA12 <- apply0(A, 1:2, mean)
##' sdA12 <- apply0(A, 1:2, sd)
##' ctrArray <- function(a,mu) return(a-mu)
##' sweep0(A, numeric(0), meanA0, ctrArray)
##' sweep0(A, 1:2, meanA12, ctrArray)
##' @export
sweep0 <- function(X, MARGIN, STATS, FUN, ...){
  if(length(MARGIN)==0 & length(STATS)==1)
    return(FUN(X, STATS))
  if(length(MARGIN)==0 & length(STATS)>1)
    warning("length(STATS) or dim(STATS) do not match dim(x)[MARGIN]")
  return(sweep(X, MARGIN, STATS, FUN, ...))
}

##' @rdname apply_sweep
##' @examples
##' statsA12 <- list(meanA12, sdA12)
##' stdArray <- function(a,mu,sigma) return((a-mu)/sigma)
##' msweep(A, 1:2, statsA12, stdArray)
##' @export
msweep <- function (X, MARGIN, STATS, FUN, 
          ...) 
{
  check.margin = TRUE
  FUN <- match.fun(FUN)
  dims <- dim(X)
  if (check.margin) {
    dimmargin <- dims[MARGIN]
    dimstats <- dim(STATS[[1]])
    lstats <- length(STATS[[1]])
    if (lstats > prod(dimmargin)) {
      warning("STATS is longer than the extent of 'dim(x)[MARGIN]'")
    }
    else if (is.null(dimstats)) {
      cumDim <- c(1L, cumprod(dimmargin))
      upper <- min(cumDim[cumDim >= lstats])
      lower <- max(cumDim[cumDim <= lstats])
      if (lstats && (upper%%lstats != 0L || lstats%%lower != 
                     0L)) 
        warning("STATS does not recycle exactly across MARGIN")
    }
    else {
      dimmargin <- dimmargin[dimmargin > 1L]
      dimstats <- dimstats[dimstats > 1L]
      if (length(dimstats) != length(dimmargin) || any(dimstats != 
                                                       dimmargin)) 
        warning("length(STATS) or dim(STATS) do not match dim(x)[MARGIN]")
    }
  }
  if(length(MARGIN)>0){
    perm <- c(MARGIN, seq_along(dims)[-MARGIN])
    STATS <- Map(function(ArrList) aperm(array(ArrList, dims[perm]), order(perm)), STATS)
  }
  do.call(FUN, c(list(X), STATS))
}