% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unc_postprocess.R
\name{unc_postprocess}
\alias{unc_postprocess}
\alias{ppud}
\alias{min_lambda_calc}
\title{Postprocess UD}
\usage{
ppud(UD, lambda_list)

min_lambda_calc(UD, prop)
}
\arguments{
\item{UD}{model-wise uncertainty or stage-wise uncertainty}

\item{lambda_list}{a numeric vector that adjust the degree to which uncertainties and average uncertainty are close.}

\item{prop}{target proportion of least uncertainty}
}
\description{
ppud() adjusts uncertainty so that it is not less than a certain value. In particular, it is often used for UD_bal_model, where the uncertainty may be negative.
min_lambda_calc() finds lambda such that the proportion of the minimum uncertainty is the specified value.
}
\examples{
set.seed(0)
stage1 <- LETTERS[1:3]
stage2 <- LETTERS[1:2]
stage3 <- LETTERS[1:4]
y <- rnorm(3*2*4)
data <- expand.grid(stage1=stage1,
                    stage2=stage2,
                    stage3=stage3)
stages <- names(data)
data <- cbind(data, y)

UD_bal_model_var <- UD_bal_model(data, "y", stages, u_var, flist_var)
UD_bal_model_var
UD_bal_model_mad <- UD_bal_model(data, "y", stages, u_mad, flist_mad)
UD_bal_model_mad 
UD_bal_model_range <- UD_bal_model(data, "y", stages, u_range, flist_range)
UD_bal_model_range 

UD_bal_stage_var <- UD_model2stage(UD_bal_model_var)
UD_bal_stage_var 
UD_bal_stage_mad <- UD_model2stage(UD_bal_model_mad)
UD_bal_stage_mad 
UD_bal_stage_range <- UD_model2stage(UD_bal_model_range)
UD_bal_stage_range 

UD_list = ppud(UD_bal_model_range)
plot(UD_list)

lambda = min_lambda_calc(UD_bal_model_range, 0.01)
UD_1percent = ppud(UD_bal_model_range, lambda)
UD_1percent$UD[[1]]

}
