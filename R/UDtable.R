#' Make uncertainty table
#'
#' This function summarizes the uncertainty into a table.
#'
##' @name UDtable
##' @rdname UDtable
##'
#' @param UD model-wise uncertainty or stage-wise uncertainty
#' @param include.nat If include.nat is TRUE and UD has nat_unc, create a table containing it.
#' @param include.tot If include.tot is TRUE, create a table containing it.
#' @param ... further arguments passed to or from other methods. include.nat or include.tot
#' @return uncertainty table
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
#' UD_bal_model_range <- UD_bal_model(data, "y", stages, u_range, flist_range)
#' UD_bal_model_range 
#' 
#' UD_bal_stage_range <- UD_model2stage(UD_bal_model_range)
#' UD_bal_stage_range 
#' 
#' UD_table(UD_bal_model_range)
#' UD_table(UD_bal_stage_range)



##' @rdname UDtable
#' @export
UD_table <- function(UD, ...){
  UseMethod('UD_table') 
}

##' @rdname UDtable
#' @export
UD_table.UD_model <- function(UD, include.nat=TRUE, include.tot=TRUE, ...){
  ElemNames <- Map(function(x) Map(names, x), UD)
  ElemUnc <- mapply(identical, ElemNames, ElemNames['unc'])
  ElemUnc <- ElemUnc & mapply(function(x) all(mapply(is.numeric, x)), UD)
  ElemOthUnc <- ElemUnc & (names(UD)!="unc")
  ElemOthUnc  <- names(UD)[ElemOthUnc ]
  ElemUnc  <- names(UD)[ElemUnc ]
  ElemPrint <- c("stage", "model", ElemOthUnc, "unc")
  
  result_table = do.call(function(...) Map(data.frame,...), UD[ElemPrint])
  result_table = do.call(rbind, result_table)
  rownames(result_table)=NULL
  
  if(!is.null(UD$nat_unc) & include.nat){
    result_table_nat <- result_table[1,]
    result_table_nat$stage="Internal"
    result_table_nat$model=""
    result_table_nat[ElemOthUnc] = 0
    result_table_nat$unc=UD$nat_unc
    result_table = rbind(result_table, result_table_nat)
  } 
  if(include.tot){
    result_table_tot<- result_table[1,]
    result_table_tot$stage="Total"
    result_table_tot$model=""
    result_table_tot[ElemUnc] = colSums(result_table[ElemUnc])
    
    result_table = rbind(result_table, result_table_tot)
  }
  
  result_table$prop = result_table$unc/UD$tot_unc
  return(result_table)
}


##' @rdname UDtable
#' @export
UD_table.UD_stage <- function(UD, include.nat=TRUE, include.tot=TRUE, ...){
  ElemNames <- Map(names, UD)
  ElemUnc <- mapply(identical, ElemNames, ElemNames['unc'])
  ElemUnc <- ElemUnc & mapply(is.numeric, UD)
  ElemOthUnc <- ElemUnc & (names(UD)!="unc")
  ElemOthUnc  <- names(UD)[ElemOthUnc ]
  ElemUnc  <- names(UD)[ElemUnc ]
  ElemPrint <- c("stage", ElemOthUnc, "unc")
  
  result_table = do.call(data.frame, UD[ElemPrint])
  rownames(result_table)=NULL
  
  if(!is.null(UD$nat_unc) & include.nat){
    result_table_nat <- result_table[1,]
    result_table_nat$stage="Internal"
    result_table_nat[ElemOthUnc] = 0
    result_table_nat$unc=UD$nat_unc
    result_table = rbind(result_table, result_table_nat)
  } 
  if(include.tot){
    result_table_tot<- result_table[1,]
    result_table_tot$stage="Total"
    result_table_tot[ElemUnc] = colSums(result_table[ElemUnc])
    if("cum_unc" %in% ElemUnc)
      result_table_tot$cum_unc = UD$tot_unc
    result_table = rbind(result_table, result_table_tot)
  }
  
  
  result_table$prop = result_table$unc/UD$tot_unc
  return(result_table)
}
