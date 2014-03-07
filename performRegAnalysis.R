#' Analyzing Covariate Choice Consequences
#'
#' Uses output of the \code{\link{fitRegCombin}} function to determine the importantance of coefficients 
#'
#' @param x a formal class RegCombin object
#'
#' @return An object of class RegAnalysis containing
#' \itemize{
#'  \item{MeanR2}{A numeric vector of length n+1, where n is the number of columns in \code{X}.  Each element is the mean R^2 value for all regressions on combinations of covaraites containing a given covariate.  For example, the first element is the mean R^2 for all regressions of combinations of covariates which contains the intercept.}
#'  \item{coefficients}{A n by (2^n)-1 matrix of coefficient values, where n is the number of columns in \code{X} plus 1 (for the intercept).  Each column in this object represeents the output of a single model, each row represents a given variable in \code{X}.}
#'  \item{R2}{A numeric vector of length (2^n)-1 of R^2 values, where n is the number of columns in \code{X}.}
#'  \item{X}{The first object input} 
#'  \item{y}{The second object input}
#'  }
#'  
#' @author Dalston G. Ward  \email{ward.dalston@gmail.com}
#' @note The reason that the number of combinations is (2^n)-1 instead of 2^n is that one cannot fit a regression with no covariates and no intercept (which is the combination of none of the columns of \code{X}.
#' @examples
#' 
#' set.seed(1801)
#' myX <- matrix(rpois(n=60,lambda=15),ncol=4)
#' myY <- sample(1:100,15,replace=TRUE) 
#' myRegCombin <- fitRegCombin(myX, myY)
#' performRegAnalysis(myRegCombin)
#' @rdname performRegAnalysis
#' @aliases performRegAnalysis,Regcombin-method
#' @export
setGeneric(name="performRegAnalysis",
           def=function(object)
           {standardGeneric("performRegAnalysis")}
)

#' @export
setMethod(f="performRegAnalysis",
          signature="RegCombin",
          definition=function(object){
          MeanR2 <- apply(object@coefficients,1,function(x) mean(object@R2[which(!is.na(x))] ) )
            return(new("RegAnalysis",MeanR2=MeanR2,coefficients=object@coefficients,R2=object@R2,X=object@X,y=object@y))
          } # close definition 
) #close set method            