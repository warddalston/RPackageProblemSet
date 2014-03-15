\docType{class}
\name{RegCombin-class}
\alias{RegCombin-class}
\alias{getRegCombin}
\alias{getRegCombin,RegCombin-method}
\alias{getRegCombinInput}
\alias{initialize,RegCombin-method}
\title{Regressions of all covaraite combinations}
\usage{
getRegCombin(object)

getRegCombinInput(object)
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
Dalston G. Ward: \email{ward.dalston
}

