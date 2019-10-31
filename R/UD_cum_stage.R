#' Cumulative uncertainty
#'
#' This function performs uncertainty decomposition based on the cumulative uncertainty.
#' @param data a data frame containing models(factor or character) for each stages and the variable of interest(numeric).
#' data should contain all combinations of models.
#' @param var_name the name of the variable of interest
#' @param stages names of the stages in the modeling chain. should be ordered by the order of the modeling chain
#' @param U a function that returns uncertainty such as range and variance of a given numeric vector.
#' This package have built-in uncertainty functions U_var(), U_mad() and U_range(). Default is U_var().
#' @return stage wise uncertainties
#' @import stats
#' @export
#' @examples
#' set.seed(0)
#' stage1 <- LETTERS[1:3]
#' stage2 <- LETTERS[1:2]
#' stage3 <- LETTERS[1:4]
#' y <- rnorm(3*2*4)
#' data <- expand.grid(stage1=stage1,
#'                     stage2=stage2,
#'                     stage3=stage3)
#' data <- cbind(data, y)
#'
#' UD_cum_stage(data, "y", names(data)[-4], U_var)
#' UD_cum_stage(data, "y", names(data)[-4], U_mad)
#' UD_cum_stage(data, "y", names(data)[-4], U_range)

UD_cum_stage<-function(data, var_name,
                       stages=setdiff(names(data),var_name),
                       U=U_var){
  nstage <- length(stages)
  data[stages] <- lapply(data[stages], as.factor)
  
  cu <- rep(0, nstage)
  for(k in 1:(nstage-1)) {
    given_stages <- paste(stages[(k+1):nstage], collapse="+")
    formula0 <- as.formula(paste(var_name, "~" , given_stages, sep=""))
    agg <- aggregate(formula0, data=data, U)
    cu[k] <- mean(agg[[ var_name ]])
  }
  cu[nstage] <- U(data[[ var_name ]])
  u_cum = c(cu[1], diff(cu))
  names(u_cum) = stages
  names(cu) = stages
  
  tot_unc = cu[nstage]
  result <- list(unc=u_cum,
                 cum_unc=cu,
                 tot_unc=tot_unc,
                 stage=stages)
  
  class(result) = "UD_stage"
  return(result)
}

