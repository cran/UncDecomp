#' Convert model uncertainty to stage uncertainty
#'
#' This function converts model uncertainty to stage uncertainty by summing by stage.
#'
#' @param UD model wise uncertainty, output of function that creates model wise uncertainty such as UD_bal_model and UD_ANOVA_model
#' @return stage wise uncertainties 
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
#' UD_bal_model_var <- UD_bal_model(data, "y", names(data)[-4], u_var, flist_var)
#' UD_bal_model_var
#' UD_bal_model_mad <- UD_bal_model(data, "y", names(data)[-4], u_mad, flist_mad)
#' UD_bal_model_mad 
#' UD_bal_model_range <- UD_bal_model(data, "y", names(data)[-4], u_range, flist_range)
#' UD_bal_model_range 
#'
#' UD_bal_stage_var <- UD_model2stage(UD_bal_model_var)
#' UD_bal_stage_var 
#' UD_bal_stage_mad <- UD_model2stage(UD_bal_model_mad)
#' UD_bal_stage_mad 
#' UD_bal_stage_range <- UD_model2stage(UD_bal_model_range)
#' UD_bal_stage_range 



#' @export
UD_model2stage <- function(UD){
  ElemNames <- Map(function(x) Map(names, x), UD)
  ElemUnc <- mapply(identical, ElemNames, ElemNames['unc'])
  ElemUnc <- ElemUnc & mapply(function(x) all(mapply(is.numeric, x)), UD)
  ElemUnc  <- names(UD)[ElemUnc]
  
  UD[ElemUnc] <- Map(function(x) sapply(x, sum), UD[ElemUnc])
  UD["model"] <- NULL
  
  class(UD) = "UD_stage"
  return(UD)
}

