\docType{methods}
\name{print,RegAnalysis-method}
\alias{print,RegAnalysis-method}
\title{Printing RegAnalysis objects}
\usage{
print(x,...)
}
\arguments{
  \item{x}{an object of class `RegAnalysis'}

  \item{...}{further arguments passed to or from other
  methods.}
}
\description{
Prints a summary of important information form an object of
class `RegAnalysis'
}
\details{
The print method for objects of class `RegAnalysis' prints
to the console the following information about a
`RegAnalysis' object: the number of observations per
regression, the number of regressions run, the number of
covariates considered, the highest R^2 value, the variables
included in that regression, the average R^2 value for each
regressions including each covariate, the variance of R^2
values for each covariate, as well as the average value for
each of the included coefficents.
}
\note{
The print and show methods for objects of class
`RegAnalysis' do NOT return the same output. Means and
variances of R^2 rounded to four digits.
}
\author{
Dalston G. Ward
}
\seealso{
\code{\link{show,RegAnalysis-method}}
}

