#' A Regression Analysis 
#' 
#' Object of class \code{RegAnalysis} are created by the \code{fitRegAnalysis} function
#'
#' 
#' An object of the class `RegAnalysis' has the following slots:
#' \itemize{
#' \item \code{coefficients} A matrix of the regression coefficients created by regressions of all possible combinations of \code{X} columns on \code{y}
#' \item \code{R2} A vector of R^2 values from the regressions of all possible combinations of \code{X} on \code{y}
#' \item \code{X} The first input, a matrix of covariate values (intercept vector not included)
#' \item \code{y} the second input, a numeric vector of the same length as the number of rows of \code{X} 
#' }
#'
#' @author Dalston G. Ward: \email{ward.dalston@gmail.com}
#' @aliases RegAnalysis-class initialize,RegAnalysis-method getRegAnalysis,RegAnalysis-method 
#' @rdname RegAnalysis
#' @export
setClass(Class="RegAnalysis", 
         slots = list(
           coefficients = "matrix",
           R2 = "numeric",
           X = "matrix",
           y = "numeric"
         ),
         prototype = prototype(
           coefficients = matrix(),
           R2 = numeric(),
           X = matrix(),
           y = numeric()
         )
)

#' @export
setMethod("initialize", "RegAnalysis", 
          function(.Object, coefficients=matrix(0,nrow=1,ncol=1), R2=numeric(0),X=matrix(0,nrow=1,ncol=1),y=numeric(0)){
            if(any(is.na(X))){
              stop("The function does not accept covariate matrices with missing values")
            }
            if(any(is.na(y))){
              stop("The function does not accept outcome vectors with missing values")
            }
            if(length(y)!=nrow(X)){
              stop("The length of y and the number of rows of X must be equal")
            }
            .Object@coefficients <- coefficients
            .Object@R2 <- R2
            .Object@X <- X
            .Object@y <- y
            .Object
          }
) 

#' @rdname RegAnalysis
#' @export 
setGeneric("getRegAnalysis",
           function(object="RegAnalysis")  {
             standardGeneric("getRegAnalysis")
           }
)

#' @export
setMethod("getRegAnalysis", "RegAnalysis",
          function(object){ 
            return(list(coefficients=object@coefficients,R2=object@R2))
          }
)
