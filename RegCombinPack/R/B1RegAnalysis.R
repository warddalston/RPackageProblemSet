#' A Regression Analysis object
#' 
#' Objects of class \code{RegAnalysis} are created by the \code{performRegAnalysis} function
#'
#' 
#' An object of the class `RegAnalysis' has the following slots:
#' \itemize{
#' \item \code{MeanR2} A vector of the mean r^2 values for regressions including each of the possible covariates. 
#' \item \code{VarR2} A vector of the Variance of the R^2 values for regressions including each of the covariates. 
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
         contains="RegCombin",
         slots = list(
           MeanR2 = "numeric",
           VarR2 = "numeric",
           coefficients = "matrix",
           R2 = "numeric",
           X = "matrix",
           y = "numeric"
         ),
         prototype = prototype(
           MeanR2 = numeric(),
           VarR2 = numeric(),
           coefficients = matrix(),
           R2 = numeric(),
           X = matrix(),
           y = numeric()
         )
)

#' @export
setMethod("initialize", "RegAnalysis", 
          function(.Object, MeanR2=numeric(0), VarR2=numeric(0), coefficients=matrix(0,nrow=1,ncol=1), R2=numeric(0),X=matrix(0,nrow=1,ncol=1),y=numeric(1)){
            if(any(is.na(X))){
              stop("The function does not accept covariate matrices with missing values")
            }
            if(any(is.na(y))){
              stop("The function does not accept outcome vectors with missing values")
            }
            if(length(y)!=nrow(X)){
              stop("The length of y and the number of rows of X must be equal")
            }
            .Object@MeanR2 <- MeanR2
            .Object@VarR2 <- VarR2
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
           function(object)  {
             standardGeneric("getRegAnalysis")
           }
)

#' @export
setMethod(f="getRegAnalysis", #f is some generic method that R knows (it knows getRegAnalysis because we just taught it to R in the function right about here!!!!! )
          signature="RegAnalysis", #now we teach R what to do when it sees getRegAnalysis and the input is of class RegAnalysis! 
          definition=function(object){ 
            return(list(MeanR2=object@MeanR2,VarR2=object@VarR2))
          }
)