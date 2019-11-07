#' Model-wise balanced uncertainty
#'
#' This function performs the balanced uncertainty decomposition. 
#' In balanced uncertainty decomposition, we assume that the total uncertainty decomposes into the uncertainty of all main effects and all orders of interaction between models.
#' This method distributes the uncertainties of each element evenly among the associated models.
#'
#' @param data a data frame containing models(factor or character) for each stages and the variable of interest(numeric).
#' data should contain all combinations of models.
#' @param var_name the name of the variable of interest
#' @param stages names of the stages of interest.
#' @param u a function that returns uncertainty of each element of the vector like difference or square of difference between each element and summary statistics.
#' This package have built-in uncertainty functions u_var(), u_mad() and u_range(). Default is u_var().
#' @param flist list of functions that summarize vector like mean or median.
#' This package have built-in uncertainty functions flist_var(), flist_mad() and flist_range(). Default is flist_var().
#' @return model-wise uncertainties(UD_model class)
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



UD_bal_model = function(data, var_name, stages, u=u_var, flist=flist_var){
  
  data$Y = data[[var_name]]
  K = length(stages)
  
  formulaAr = formula(paste0(var_name, "~."))
  dataAr = xtabs(formulaAr, data[c(var_name, stages)], drop.unused.levels = FALSE)
  dimnamesX = dimnames(dataAr)
  dimX = sapply(dimnamesX, length)
  m = prod(dimX) # the number of projected values
  model_unc = rep(list(NULL),K)
  names(model_unc) = stages
  
  dimA = rep(2,K)
  dimnamesA = rep(list(c(F,T)),K)
  names(dimA) = names(dimnamesA) = paste0(stages, "_A")
  unc_x_A = array(0, dim = c(dimA, dimX), dimnames = c(dimnamesA, dimnamesX))
  cardA = Reduce(function(x,y) outer(x, y, "+"), dimnamesA)
  dimnames(cardA) = dimnamesA
  
  coef_Vec = get_coef_Vec(K)
  
  powerA = as.matrix(expand.grid(dimnamesA))
  for(i in 1:nrow(powerA)){
    A = which(powerA[i,])
    fA = Map(function(f) apply0(dataAr, A, f), flist)
    unc = msweep(dataAr, A, fA, u)
    unc_x_A = sliceAssinArr(unc_x_A, unc, 1:K, as.character(powerA[i,]))
  }
  
  dimnamesA = Map(as.character, dimnamesA)
  emptyIdx = Map("[", dimnamesA, 1)
  unc_x_A = sweep0(unc_x_A, K + 1:K, 
                   sliceArr(unc_x_A, 1:K, emptyIdx), 
                   function(x,y) return(y-x))
  
  for(k in 1:K){
    notinA = Map("[", dimnamesA[k], 1)
    inA = Map("[", dimnamesA[k], 2)
    sumk = apply(coef_Vec[sliceArr(cardA, k, inA)] * sliceArr(unc_x_A, k, inA), (K-1) + k, sum)
    sumNotk = apply(coef_Vec[sliceArr(cardA, k, notinA) + 1] * sliceArr(unc_x_A, k, notinA), (K-1) + k, sum)
    unc_k = (sumk - sumNotk)/m
    model_unc[[k]] = unc_k
  }
  tot_unc = do.call(sum, model_unc)
  result = list(unc=model_unc,
                tot_unc=tot_unc,
                stage=stages,
                model=dimnamesX)
  class(result) = "UD_model"
  return(result)
}



