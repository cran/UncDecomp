##' Postprocess UD
##'
##' ppud() adjusts uncertainty so that it is not less than a certain value. In particular, it is often used for UD_bal_model, where the uncertainty may be negative.
##' min_lambda_calc() finds lambda such that the proportion of the minimum uncertainty is the specified value.
##'
##' @name unc_postprocess
##' @rdname unc_postprocess
##'
##' @param UD model-wise uncertainty or stage-wise uncertainty
##' @param lambda_list a numeric vector that adjust the degree to which uncertainties and average uncertainty are close.
##' @param prop target proportion of least uncertainty
##'
NULL

##' @rdname unc_postprocess
##' @examples
#' set.seed(0)
#' stage1 <- LETTERS[1:3]
#' stage2 <- LETTERS[1:2]
#' stage3 <- LETTERS[1:4]
#' y <- rnorm(3*2*4)
#' data <- expand.grid(stage1=stage1,
#'                     stage2=stage2,
#'                     stage3=stage3)
#' stages <- names(data)
#' data <- cbind(data, y)
#'
#' UD_bal_model_var <- UD_bal_model(data, "y", stages, u_var, flist_var)
#' UD_bal_model_var
#' UD_bal_model_mad <- UD_bal_model(data, "y", stages, u_mad, flist_mad)
#' UD_bal_model_mad 
#' UD_bal_model_range <- UD_bal_model(data, "y", stages, u_range, flist_range)
#' UD_bal_model_range 
#'
#' UD_bal_stage_var <- UD_model2stage(UD_bal_model_var)
#' UD_bal_stage_var 
#' UD_bal_stage_mad <- UD_model2stage(UD_bal_model_mad)
#' UD_bal_stage_mad 
#' UD_bal_stage_range <- UD_model2stage(UD_bal_model_range)
#' UD_bal_stage_range 
#' 
#' UD_list = ppud(UD_bal_model_range)
#' plot(UD_list)
#'
#' lambda = min_lambda_calc(UD_bal_model_range, 0.01)
#' UD_1percent = ppud(UD_bal_model_range, lambda)
#' UD_1percent$UD[[1]]
#'
##' @rdname unc_postprocess
##' @export
ppud = function(UD, lambda_list){
  UDdf = do.call(function(...) Map(data.frame,...), UD[c("stage", "unc")])
  UDdf = do.call(rbind, UDdf)

  w = unlist(UDdf$unc)
  U = sum(w)
  v = U/length(w)
  if(missing(lambda_list)){
    lambda_list = exp(seq(-5, 5, length.out = 35))
  }
  UDlist = list()
  UDlist$lambda = lambda_list
  UDlist$UD = rep(list(UD), length(lambda_list))

  for(i in seq_along(lambda_list)){
    lambda=lambda_list[i]
    if(is.infinite(lambda)){
      theta = rep(v, length(w))
    } else{
      z = (w + lambda*v)/(1+lambda)
      theta = pmax(z - get_alpha(z, U), 0)
    }
    UDlist$UD[[i]]$unc = tapply(theta, UDdf$stage, identity)
  }
  class(UDlist) = "UD_list"
  return(UDlist)
}


get_alpha = function(z, U){
  z_sorted = sort(z, decreasing=TRUE)
  r = 0
  for(j in 1:length(z_sorted)){
    r = r + (sum(pmax(z-z_sorted[j],0))<U)
  }
  alpha = (sum(z_sorted[1:r])-U)/r
  return(alpha)
}


##' @rdname unc_postprocess
##' @export
min_lambda_calc = function(UD, prop){
  w = unlist(UD$unc)
  U = sum(w)
  v = U/length(w)
  if(prop> 1/length(w)){
    warning("prop is greater than 1/(number of uncertainties)")
    lambda = Inf
  }else{
    theta_min = prop*U
    lambda = (theta_min - min(w))/ (v - theta_min)
  }
  return(lambda)
}

