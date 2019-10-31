#' Scenario uncertainty based on the second order interaction ANOVA
#'
#' This function performs uncertainty decomposition by scenario based on the second order interaction ANOVA model.
#' The uncertainty from interaction effect from two scenarios is divided equally and assigned to each scenario.
#' @param data a data frame containing scenarios(factor or character) for each stages and the variable of interest(numeric).
#' data should contain all combinations of scenarios.
#' @param var_name the name of the variable of interest
#' @param stages names of the stages of interest.
#' @return List including uncertainties of models, uncertainties from main effects, uncertainties from interaction, total uncertainty, names of stages and models
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
#' UD_ANOVA_model <- UD_ANOVA_model(data, "y", names(data)[-4])
#' UD_ANOVA_model
#' UD_ANOVA_stage <- UD_model2stage(UD_ANOVA_model)
#' UD_ANOVA_stage


UD_ANOVA_model<-function(data, var_name,
                         stages=setdiff(names(data),var_name)){
  nstage <- length(stages)
  data[stages] <- lapply(data[stages], as.factor)
  data <- data[c(stages, var_name)]
  
  formulaAr = formula(paste0(var_name, "~."))
  dataAr = xtabs(formulaAr, data[c(var_name, stages)], drop.unused.levels = FALSE)
  dimnamesX = dimnames(dataAr)
  
  
  formula0 <- as.formula(paste(var_name, "~1" , sep=""))
  mean_proj <- aggregate(formula0, data=data, mean)[[var_name]]
  
  main_proj <- list()
  
  for(j in 1:nstage){
    formula0 <- as.formula(paste(var_name, "~" , stages[j], sep=""))
    main_proj[[j]] <- aggregate(formula0, data=data, mean)[[var_name]]
    names(main_proj[[j]]) <- levels(data[[stages[j]]])
  }
  names(main_proj) <- stages
  
  main_uncer <- mapply(function(x,y) (x-y)^2/length(x), main_proj, mean_proj)
  
  int_uncer <- list()
  for(j in 1:nstage){
    int_uncer[[j]] <- 0
    for(k in (1:nstage)[-j]){
      formula0 <- as.formula(paste(var_name, "~" , stages[j], "+", stages[k], sep=""))
      inter_proj <- aggregate(formula0, data=data, mean)[[var_name]]
      int_uncer0 <- ((inter_proj - main_proj[[j]] - rep(main_proj[[k]],each=nlevels(data[,j])) + mean_proj)^2)
      int_uncer0 <- tapply(int_uncer0, rep(levels(data[,j]),nlevels(data[,k])) , mean)
      int_uncer[[j]] <- int_uncer[[j]] + int_uncer0
    }
    int_uncer[[j]] <- int_uncer[[j]]/(2*nlevels(data[,j]))
  }
  names(int_uncer) <- stages
  model_uncer <- Map("+",main_uncer, int_uncer)
  result_table<-NULL
  for(j in 1:nstage){
    stage_name <- stages[j]
    result_table0 <- data.frame(stage=stage_name,
                                model=levels(data[,j]),
                                unc_from_main=main_uncer[[j]],
                                unc_from_int=int_uncer[[j]],
                                model_uncer=model_uncer[[j]])
    result_table <- rbind(result_table, result_table0)
  }
  rownames(result_table) <- NULL
  
  tot_uncer <- mean((data[[var_name]]-mean(data[[var_name]]))^2)
  natural_uncer <- tot_uncer - sum(result_table$model_uncer)
  
  result <- list(unc=model_uncer,
                 main_unc=main_uncer,
                 int_unc=int_uncer,
                 nat_unc=natural_uncer,
                 tot_unc=tot_uncer,
                 stage=stages,
                 model=dimnamesX)
  class(result) <- "UD_model"
  return(result)
}

