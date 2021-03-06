% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UD_cum_stage.R
\name{UD_cum_stage}
\alias{UD_cum_stage}
\title{Stage-wise uncertainty based on cumulative uncertainty}
\usage{
UD_cum_stage(data, var_name, stages = setdiff(names(data), var_name),
  U = U_var)
}
\arguments{
\item{data}{a data frame containing models(factor or character) for each stages and the variable of interest(numeric).
data should contain all combinations of models.}

\item{var_name}{the name of the variable of interest}

\item{stages}{names of the stages in the modeling chain. should be ordered by the order of the modeling chain}

\item{U}{a function that returns uncertainty such as range and variance of a given numeric vector.
This package have built-in uncertainty functions U_var(), U_mad() and U_range(). Default is U_var().}
}
\value{
stage-wise uncertainties(UD_stage class)
}
\description{
This function performs uncertainty decomposition based on the cumulative uncertainty.
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

UD_cum_stage(data, "y", stages, U_var)
UD_cum_stage(data, "y", stages, U_mad)
UD_cum_stage(data, "y", stages, U_range)
}
