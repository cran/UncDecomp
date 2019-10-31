#' Stage wise balanced uncertainty
#'
#' This function performs the balanced uncertainty decomposition. 
#' In balanced uncertainty decomposition, we assume that the total uncertainty decomposes into the uncertainty of all main effects and all orders of interaction between stages.
#' This method distributes the uncertainties of each element evenly among the associated stages.
#'
#' @param data a data frame containing models(factor or character) for each stages and the variable of interest(numeric).
#' data should contain all combinations of models.
#' @param var_name the name of the variable of interest
#' @param stages names of the stages of interest.
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
#' UD_bal_stage(data, "y", names(data)[-4], U_var)
#' UD_bal_stage(data, "y", names(data)[-4], U_mad)
#' UD_bal_stage(data, "y", names(data)[-4], U_range)




UD_bal_stage = function(data, var_name, stages, U=U_var){
  
  data$Y = data[[var_name]]
  K = length(stages)
  
  formulaAr = formula(paste0(var_name, "~."))
  dataAr = xtabs(formulaAr, data[c(var_name, stages)], drop.unused.levels = FALSE)
  dimnamesX = dimnames(dataAr)
  dimX = sapply(dimnamesX, length)
  m = prod(dimX) # the number of projected values
  stage_unc = vector("numeric", K)
  names(stage_unc) = stages
  
  dimA = rep(2,K)
  dimnamesA = rep(list(c(F,T)),K)
  names(dimA) = names(dimnamesA) = paste0(stages, "_A")
  unc_A = array(0, dim = dimA, dimnames = dimnamesA)
  cardA = Reduce(function(x,y) outer(x, y, "+"), dimnamesA)
  dimnames(cardA) = dimnamesA
  
  coef_Vec = get_coef_Vec(K)
  
  powerA = as.matrix(expand.grid(dimnamesA))
  for(i in 1:nrow(powerA)){
    A = which(powerA[i,])
    unc = mean(apply0(dataAr, A, U))
    unc_A = sliceAssinArr(unc_A, unc, 1:K, as.character(powerA[i,]))
  }
  
  dimnamesA = Map(as.character, dimnamesA)
  emptyIdx = Map("[", dimnamesA, 1)
  unc_A = sweep0(unc_A, 1:K, 
                 sliceArr(unc_A, 1:K, emptyIdx), 
                 function(x,y) return(y-x))
  
  for(k in 1:K){
    notinA = Map("[", dimnamesA[k], 1)
    inA = Map("[", dimnamesA[k], 2)
    sumk = sum(coef_Vec[sliceArr(cardA, k, inA)] * sliceArr(unc_A, k, inA))
    sumNotk = sum(coef_Vec[sliceArr(cardA, k, notinA) + 1] * sliceArr(unc_A, k, notinA))
    unc_k = (sumk - sumNotk)
    stage_unc[k] = unc_k
  }
  
  tot_unc = sum(stage_unc)
  result <- list(unc=stage_unc,
                 tot_unc=tot_unc,
                 stage=stages)
  
  class(result) = "UD_stage"
  
  return(result)
}


get_coef = function(K, b){
  x = choose(K-b, 0:(K-b))/((b):K)
  xx = x*(-1)^(0:(K-b))
  return(sum(xx))
}

get_coef_Vec = function(K){
  return(sapply(1:K, function(b) get_coef(K, b)))
}



sliceArr <- function(x, DimVec, IdxList){
  ndim <- length(dim(x))
  FullIdxList <- rep(list(T), ndim)
  FullIdxList[DimVec] <- IdxList
  
  return(do.call("[", 
                 c(list(x), FullIdxList)))
}

sliceAssinArr <- function(x, y, DimVec, IdxList){
  ndim <- length(dim(x))
  FullIdxList <- rep(list(T), ndim)
  FullIdxList[DimVec] <- IdxList
  
  return(do.call("[<-",
                 c(list(x), FullIdxList, list(y))
  )
  )
}

