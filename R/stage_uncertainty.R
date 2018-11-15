#' Stage uncertainty
#'
#' This function performs uncertainty decomposition by stage based on the second order interaction ANOVA model.
#' The uncertainty from interaction effect from two stages is divided equally and assigned to each stage.
#' @param data a data frame containing scenarios(factor or character) for each stages and the variable of interest(numeric).
#' data should contain all combinations of scenarios. columns scenarios
#' @param var_name the name of the variable of interest
#' @param stages names of the stages in the model.
#' @return List of 4 elements
#' \item{summary}{summary of uncertainties}
#' \item{main_uncer}{a vector of uncertainties from the main effects of the stages}
#' \item{int_uncer}{a vector of uncertainties from the interaction effects of the stages}
#' \item{scenario_uncer}{a vector of uncertainties of the stages}
#' @import stats
#' @export
#' @examples
#' stage1 <- LETTERS[1:3]
#' stage2 <- LETTERS[1:2]
#' stage3 <- LETTERS[1:4]
#' y <- rnorm(3*2*4)
#' data <- expand.grid(stage1=stage1,
#'                     stage2=stage2,
#'                     stage3=stage3)
#' data <- cbind(data, y)
#' stage_uncertainty(data,"y", names(data)[-4])

stage_uncertainty<-function(data, var_name,
                            stages=setdiff(names(data),var_name)){
  nstage <- length(stages)
  data[stages] <- lapply(data[stages], as.factor)

  formula0 <- as.formula(paste(var_name, "~1" , sep=""))
  mean_proj <- aggregate(formula0, data=data, mean)[[var_name]]

  main_proj <- list()

  for(j in 1:nstage){
    formula0 <- as.formula(paste(var_name, "~" , stages[j], sep=""))
    main_proj[[j]] <- aggregate(formula0, data=data, mean)[[var_name]]
    names(main_proj[[j]]) <- levels(data[[stages[j]]])
  }
  names(main_proj) <- stages

  main_uncer <- mapply(function(x,y) mean((x-y)^2), main_proj, mean_proj)

  int_uncer <- matrix(0,nstage,nstage)
  for(j in 1:(nstage-1)){
    for(k in (j+1):nstage){
      formula0 <- as.formula(paste(var_name, "~" , stages[j], "+", stages[k], sep=""))
      inter_proj <- aggregate(formula0, data=data, mean)[[var_name]]
      int_uncer[j,k] = mean((inter_proj - main_proj[[j]] - rep(main_proj[[k]],each=nlevels(data[,j])) + mean_proj)^2)
    }
  }
  uncer_from_int <- rowSums((int_uncer + t(int_uncer))/2)
  names(uncer_from_int) <- stages
  stage_uncer <- main_uncer + uncer_from_int

  tot_uncer <- mean((data[[var_name]]-mean(data[[var_name]]))^2)
  natural_uncer <- tot_uncer - sum(stage_uncer)

  result_table <- data.frame(stage=stages,
                       uncer_from_main=main_uncer,
                       uncer_from_int=uncer_from_int,
                       stage_uncer=stage_uncer)

  result_table0 <- data.frame(stage=c("Internal", "Total"),
                        uncer_from_main=c(0, 0),
                        uncer_from_int=c(0, 0),
                        stage_uncer=c(natural_uncer, tot_uncer))
  result_table <- rbind(result_table, result_table0)
  result_table$Prop <- result_table$stage_uncer/tot_uncer
  rownames(result_table) <- NULL

  result <- list(summary=result_table,
                 main_uncer=main_uncer,
                 int_uncer=uncer_from_int,
                 stage_uncer=stage_uncer)

  return(result)
}

