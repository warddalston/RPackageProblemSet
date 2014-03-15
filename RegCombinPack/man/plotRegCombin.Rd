\docType{methods}
\name{plot,RegCombin-method}
\alias{plot,RegCombin-method}
\title{Plotting RegCombin objects}
\usage{
plot(x,plot.int=TRUE...)
}
\arguments{
  \item{x}{an object of class `RegCombin'}

  \item{plot.int=TRUE}{a logical which determines whether
  or not to plot the intercept coefficient.}

  \item{...}{arguements passed on to other functions.}
}
\description{
Plots some of the output of a RegCombin object
}
\details{
The plot method for objects of class `RegCombin' plots on
the the average coefficient values for each of the
covariates included in the fitting of regressions including
all possible combinations of regression covariates.
Covariates are plotted along the x-axis.  Coefficient
estimates are plotted along the y-axis.  The average
coefficient estimate is plotted, along with lines
indicating the coefficient estimates covered within 2
standard deviations of the mean estimate.  The second
argument, plot.int, allows the user to opt not to plot the
intercept's coefficient.  This is useful when the user is
not interested in the intercept or when the intercept has a
very different value then other covariates.
}
\examples{
set.seed(1801)
myX <- matrix(rpois(n=60,lambda=15),ncol=4)
myY <- sample(1:100,15,replace=TRUE)
myRegCombin <- fitRegCombin(myX, myY)
plot(myRegCombin,plot.int=FALSE)
}
\seealso{
\code{\link{RegCombin-method}}
}

