\name{performRegAnalysis}
\alias{performRegAnalysis}
\alias{performRegAnalysis,Regcombin-method}
\title{Analyzing Covariate Choice Consequences}
\usage{
performRegAnalysis(object)
}
\arguments{
  \item{object}{a formal class RegCombin object}
}
\value{
An object of class RegAnalysis containing \itemize{
\item{MeanR2}{ A numeric vector of length n+1, where n is
the number of columns in \code{X}.  Each element is the
mean R^2 value for all regressions on combinations of
covaraites containing a given covariate.  For example, the
first element is the mean R^2 for all regressions of
combinations of covariates which contains the intercept.}
\item{VarR2}{ A numeric vector of length n+1, where n is
the number of columns in \code{X}.  Each element is the
variance of the R^2 values for all regressions including a
given variable as a covariate.} \item{coefficients}{ A n by
(2^n)-1 matrix of coefficient values, where n is the number
of columns in \code{X} plus 1 (for the intercept).  Each
column in this object represeents the output of a single
model, each row represents a given variable in \code{X}.}
\item{R2}{ A numeric vector of length (2^n)-1 of R^2
values, where n is the number of columns in \code{X}.}
\item{X}{ The first object input} \item{y}{ The second
object input} }
}
\description{
Uses output of the \code{\link{fitRegCombin}} function to
determine the importantance of coefficients
}
\note{
The reason that the number of combinations is (2^n)-1
instead of 2^n is that one cannot fit a regression with no
covariates and no intercept (which is the combination of
none of the columns of \code{X}.
}
\examples{
set.seed(1801)
myX <- matrix(rpois(n=60,lambda=15),ncol=4)
myY <- sample(1:100,15,replace=TRUE)
myRegCombin <- fitRegCombin(myX, myY)
performRegAnalysis(myRegCombin)
}
\author{
Dalston G. Ward
}

