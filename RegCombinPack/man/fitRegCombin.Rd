\name{fitRegCombin}
\alias{fitRegCombin}
\alias{fitRegCombin,ANY-method}
\title{Regressions with all possible covariave combinations}
\usage{
fitRegCombin(X, y)
}
\arguments{
  \item{X}{A numeric matrix object of covariates with no
  missing values (should not include the intercept vector)}

  \item{y}{A numeric object with the same number of
  elements as \code{X} has rows.}
}
\value{
An object of class RegCombin containing \itemize{
\item{coefficients}{ A n by (2^n)-1 matrix of coefficient
values, where n is the number of columns in \code{X} plus 1
(for the intercept).  Each column in this object
represeents the output of a single model, each row
represents a given variable in \code{X}.} \item{R2}{ A
numeric vector of length (2^n)-1 of R^2 values, where n is
the number of columns in \code{X}.} \item{X}{ The first
object input} \item{y}{ The second object input} }
}
\description{
Regresses all possible combinations of a covariates on an
outcome variable
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
fitRegCombin(myX, myY)
}
\author{
Dalston G. Ward \email{ward.dalston@gmail.com}
}

