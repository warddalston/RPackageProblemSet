\docType{class}
\name{RegCombin-class}
\alias{RegCombin-class}
\alias{getRegCombin}
\alias{getRegCombin,RegCombin-method}
\alias{getRegCombinInput}
\alias{getRegCombinInput,RegCombin-method}
\alias{initialize,RegCombin-method}
\alias{performRegAnalysis,RegCombin-method}
\title{A Regression Combination object}
\usage{
\S4method{initialize}{RegCombin}(.Object, coefficients = matrix(0, nrow = 1,
  ncol = 1), R2 = numeric(0), X = matrix(0, nrow = 1, ncol = 1),
  y = numeric(1))

getRegCombin(object)

\S4method{getRegCombin}{RegCombin}(object)

getRegCombinInput(object)

\S4method{getRegCombinInput}{RegCombin}(object)

\S4method{performRegAnalysis}{RegCombin}(object)
}
\arguments{
  \item{.Object}{ An object: see the Details section.}
}
\description{
Objects of class \code{RegCombin} are created by the
\code{fitRegCombin} function
}
\details{
An object of the class `RegCombin' has the following slots:
\itemize{ \item \code{coefficients} A matrix of the
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

