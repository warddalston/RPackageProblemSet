#' Regressions of all covaraite combinations  
#' 
#' Objects of class \code{RegCombin} are created by the \code{fitRegCombin} function
#'
#' 
#' An object of the class `RegCombin' has the following slots:
#' \itemize{
#' \item \code{coefficients} A matrix of the regression coefficients created by regressions of all possible combinations of \code{X} columns on \code{y}
#' \item \code{R2} A vector of R^2 values from the regressions of all possible combinations of \code{X} on \code{y}
#' \item \code{X} The first input, a matrix of covariate values (intercept vector not included)
#' \item \code{y} the second input, a numeric vector of the same length as the number of rows of \code{X} 
#' }
#'
#' @author Dalston G. Ward: \email{ward.dalston@gmail.com}
#' @aliases RegCombin-class initialize,RegCombin-method getRegCombin,RegCombin-method 
#' @rdname RegCombin
#' @export
setClass(Class="RegCombin", 
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
setMethod("initialize", "RegCombin", 
          function(.Object, coefficients=matrix(0,nrow=1,ncol=1), R2=numeric(0),X=matrix(0,nrow=1,ncol=1),y=numeric(1)){
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

#' @rdname RegCombin
#' @export 
setGeneric("getRegCombin",
           function(object)  {
             standardGeneric("getRegCombin")
           }
)

#' @export
setMethod(f="getRegCombin", #f is some generic method that R knows (it knows getRegCombin because we just taught it to R in the function right about here!!!!! )
          signature="RegCombin", #now we teach R what to do when it sees getRegCombin and the input is of class RegCombin! 
          definition=function(object){ 
            return(list(coefficients=object@coefficients,R2=object@R2))
          }
)
