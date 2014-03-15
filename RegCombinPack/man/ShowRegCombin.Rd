\docType{methods}
\name{show,RegCombin-method}
\alias{show,RegCombin-method}
\title{Showing RegCombin objects}
\usage{
\S4method{show}{RegCombin}(object)
}
\arguments{
  \item{object}{an object of class `RegCombin'}
}
\description{
Prints some of the information contained in a RegCombin
object into the console
}
\details{
The show method for objects of class `RegCombin' prints the
first ten values from the \code{R2} slot, the first ten
values from the \code{y} slot, the leading 5 by 5 matrix
from the \code{coefficients} slot and the leading 5 by 5
matrix from the \code{X} slot.
}
\note{
The print and show methods for objects of class `RegCombin'
do NOT return the same output. R^2 and coefficient values
are rounded to four decimal places.
}
\author{
Dalston G. Ward
}
\seealso{
\code{\link{print,RegCombin-method}}
}

