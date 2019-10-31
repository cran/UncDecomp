#' Print UD
#'
#' This function creates a table summarizing the uncertainty.
#'
##' @name printUD
##' @rdname printUD
#'
#' @param x model wise uncertainty or stage wise uncertainty 
#' @param ... further arguments passed to or from other methods.
#' 
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
#' UD_bal_model_range <- UD_bal_model(data, "y", names(data)[-4], u_range, flist_range)
#' print(UD_bal_model_range)
#'
#' UD_bal_stage_range <- UD_model2stage(UD_bal_model_range)
#' print(UD_bal_stage_range)



##' @rdname printUD
#' @export
print.UD_model <- function(x, ...){
  result_table=UD_table(x, ...)
  print(result_table)
}


##' @rdname printUD
#' @export
print.UD_stage <- function(x, ...){
  result_table=UD_table(x, ...)
  print(result_table)
}



