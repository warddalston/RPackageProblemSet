\docType{methods}
\name{print,RegCombin-method}
\alias{print,RegCombin-method}
\title{Printing RegCombin objects}
\usage{
print(x,...)
}
\arguments{
  \item{x}{an object of class `RegCombin'}

  \item{...}{further arguments passed to or from other
  methods.}
}
\description{
Prints a summary of important information form an object of
class `RegCombin'
}
\details{
The print method for objects of class `RegCombin' prints to
the console the following information about a `RegCombin'
object: the number of observations per regression, the
number of regressions run, the number of covariates
considered, the highest R^2 value and the variables
included in that regression, as well as the average value
for each of the included coefficents.
}
\note{
The print and show methods for objects of class `RegCombin'
do NOT return the same output.
}
\author{
Dalston G. Ward \email{ward.dalston@gmail.com}
}
\seealso{
\code{\link{show,RegCombin-method}}
}

