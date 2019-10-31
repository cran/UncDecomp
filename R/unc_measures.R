##' uncertainty measures
##'
##' Functions beginning with U are uncertainty measure that return a scalar given a vector, such as mean absolute deviation or variance. Functions beginning with flist are lists of functions that summarize vector like mean or median. 
##' Functions beginning with u are the uncertainty of each element of the vector, which is calculated from the elements of the vector and summary statistics of the vector, like difference or square of difference between two values.
##'
##' @name unc_measures
##' @rdname unc_measures
##'
##' @param x a vector.
##' @param m summary statistics. median for u_mad() and mean for u_var()
##' @param a,b,n summary statistics for u_range(), a and b are minimum and maximum of x. n is length of x
##'
##' @rdname unc_measures
##' @export
U_range <- function(x){diff(range(x))}
##' @rdname unc_measures
##' @export
u_range = function(x,a,b,n) n*abs(b-a)*(a==x | x==b)/2
##' @rdname unc_measures
##' @export
flist_range = list(min, max, length)


##' @rdname unc_measures

##' @export
U_mad <- function(x){mean(abs(x-median(x)))}
##' @rdname unc_measures
##' @export
u_mad = function(x,m) abs(x-m)
##' @rdname unc_measures
##' @export
flist_mad = list(median)


##' @rdname unc_measures
##' @export
U_var <- function(x){mean((x-mean(x))^2)}
##' @rdname unc_measures
##' @export
u_var = function(x,m) (x-m)^2
##' @rdname unc_measures
##' @export
flist_var = list(mean)
