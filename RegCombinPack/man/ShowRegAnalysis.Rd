\docType{methods}
\name{show,RegAnalysis-method}
\alias{show,RegAnalysis-method}
\title{Showing RegAnalysis objects}
\usage{
\S4method{show}{RegAnalysis}(object)
}
\arguments{
  \item{object}{an object of class `RegAnalysis'}
}
\description{
Prints some of the information contained in a RegAnalysis
object into the console
}
\details{
The show method for objects of class `RegAnalysis' prints
the entire MeanR2 and VarR2 vectors, the first ten values
from the \code{R2} slot, the first ten values from the
\code{y} slot, the leading 5 by 5 matrix from the
\code{coefficients} slot and the leading 5 by 5 matrix from
the \code{X} slot.
}
\note{
The print and show methods for objects of class
`RegAnalysis' do NOT return the same output. Mean R^2s, R^2
variances, R^2, and coefficient values are rounded to four
decimial places.
}
\author{
Dalston G. Ward
}
\seealso{
\code{\link{print,RegAnalysis-method}}
}

