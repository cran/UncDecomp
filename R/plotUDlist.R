##' Plot UD_list
##'
##' This function plots how ppud's result changes as lambda increases.
##' @import ggplot2
##'
##' @name plotUDlist
##' @rdname plotUDlist
##'
##' @param x output of ppud
##' @param lwd line with in plot
##' @param ... further arguments passed to or from other methods.
##'
##' @return ggplot showing how the result of ppud changes as lambda increases
##' @examples
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
#' UD_bal_model_range <- UD_bal_model(data, "y", names(data)[-4], u_range, flist_range)
#' UD_bal_model_range 
#'
#' UD_bal_stage_range <- UD_model2stage(UD_bal_model_range)
#' UD_bal_stage_range 
#' 
#' UD_model_list = ppud(UD_bal_model_range)
#' plot(UD_model_list)
#' 
#' UD_stage_list = ppud(UD_bal_stage_range)
#' plot(UD_stage_list)
##' 
utils::globalVariables(c("."))
##' @export
plot.UD_list <- function(x, lwd=1, ...){
  UDdf = Map(cbind,Map(function(z) UD_table(z, include.tot=FALSE), x$UD), lambda=x$lambda)
  UDdf = do.call(rbind, UDdf)
  lambda=UDdf$lambda
  unc=UDdf$unc
  stage=UDdf$stage
  if(class(x$UD[[1]])=="UD_stage"){
    ggplot(UDdf, aes(x=log(lambda), y=unc, col=stage)) + 
      geom_line(lwd=lwd) +  ylab("Uncertainty") + theme_bw()
  } else{
    model=UDdf$model
    ggplot(UDdf, aes(x=log(lambda), y=unc, col=stage, lty=model)) + 
      geom_line(lwd=lwd) +  ylab("Uncertainty") + theme_bw()
  }
}