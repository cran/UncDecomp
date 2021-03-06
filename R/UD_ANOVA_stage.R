#' Stage-wise uncertainty based on the second order interaction ANOVA
#'
#' This function performs uncertainty decomposition by stage based on the second order interaction ANOVA model.
#' The uncertainty from interaction effect from two stages is divided equally and assigned to each stage.
#' @param data a data frame containing models(factor or character) for each stages and the variable of interest(numeric).
#' data should contain all combinations of models.
#' @param var_name the name of the variable of interest
#' @param stages names of the stages in the model.
#' @return List(UD_stage class) including uncertainties of stages, uncertainties from main effects, uncertainties from interaction, total uncertainty, names of stages.
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
#' UD_ANOVA_stage(data,"y", stages)

UD_ANOVA_stage<-function(data, var_name,
                         stages=setdiff(names(data),var_name)){
  nstage <- length(stages)
  data[stages] <- lapply(data[stages], as.factor)
  data <- data[c(stages, var_name)]
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
  
  
  result <- list(unc=stage_uncer,
                 main_unc=main_uncer,
                 int_unc=uncer_from_int,
                 nat_unc=natural_uncer,
                 tot_unc=tot_uncer,
                 stage=stages)
  class(result) <- "UD_stage"
  return(result)
  
  
  return(result)
}

