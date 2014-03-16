\docType{class}
\name{RegAnalysis-class}
\alias{RegAnalysis-class}
\alias{getRegAnalysis}
\alias{getRegAnalysis,RegAnalysis-method}
\alias{initialize,RegAnalysis-method}
\title{A Regression Analysis object}
\usage{
\S4method{initialize}{RegAnalysis}(.Object, MeanR2 = numeric(0),
  VarR2 = numeric(0), coefficients = matrix(0, nrow = 1, ncol = 1),
  R2 = numeric(0), X = matrix(0, nrow = 1, ncol = 1), y = numeric(1))

getRegAnalysis(object)
}
\arguments{
  \item{.Object}{ An object: see the Details section.}
}
\description{
Objects of class \code{RegAnalysis} are created by the
\code{performRegAnalysis} function
}
\details{
An object of the class `RegAnalysis' has the following
slots: \itemize{ \item \code{MeanR2} A vector of the mean
R^2 values for regressions including each of the possible
covariates. \item \code{VarR2} A vector of the Variance of
the R^2 values for regressions including each of the
covariates. \item \code{coefficients} A matrix of the
regression coefficients created by regressions of all
possible combinations of \code{X} columns on \code{y} \item
\code{R2} A vector of R^2 values from the regressions of
all possible combinations of \code{X} on \code{y} \item
\code{X} The first input, a matrix of covariate values
(intercept vector not included) \item \code{y} the second
input, a numeric vector of the same length as the number of
rows of \code{X} }
}
\author{
Dalston G. Ward
}

