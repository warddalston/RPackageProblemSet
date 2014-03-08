#' Showing RegCombin objects
#' 
#' Prints some of the information contained in a RegCombin object into the console
#'
#' @param object an object of class `RegCombin'
#' 
#' @details The show method for objects of class `RegCombin' prints the first ten values from the \code{R2} slot, the first ten values from the \code{y} slot, the leading 5 by 5 matrix from the \code{coefficients} slot and the leading 5 by 5 matrix from the \code{X} slot.  
#'
#' @author Dalston G. Ward: \email{ward.dalston@gmail.com}
#' @note The print and show methods for objects of class `RegCombin' do NOT return the same output.
#' @seealso \code{\link{printRegCombin}}
#' @rdname ShowRegCombin
#' @export
setMethod(f="show",
          signature="RegCombin",
          definition=function(object){
            cat("First 10 R^2 values: \n")
            if(length(object@R2) < 10){
              print(object@R2)
            } else { print(object@R2[1:10])}
            cat("\n")
            cat("First 5 rows and columns coefficent matrix: \n")
            if(nrow(object@coefficients) < 5 & ncol(object@coefficients) < 5){
              print(object@coefficients)
            }
            if(nrow(object@coefficients) < 5 & ncol(object@coefficients) >= 5){
              print(object@coefficients[,1:5])
            }
            if(nrow(object@coefficients) >= 5 & ncol(object@coefficients) < 5){
              print(object@coefficients[1:5,])
            }
            if(nrow(object@coefficients) >= 5 & ncol(object@coefficients) >= 5){
              print(object@coefficients[1:5,1:5])
            }
            cat("\n")
            cat("First 5 rows and columns in the covariate matrix: \n")
            if(nrow(object@X) < 5 & ncol(object@X) < 5){
              print(object@X)
            }
            if(nrow(object@X) < 5 & ncol(object@X) >= 5){
              print(object@X[,1:5])
            }
            if(nrow(object@X) >= 5 & ncol(object@X) < 5){
              print(object@X[1:5,])
            }
            if(nrow(object@X) >= 5 & ncol(object@X) >= 5){
              print(object@X[1:5,1:5])
            }
            cat("\n")
            cat("First 10 outcome variable values: \n")
            if(length(object@y) < 10){
              print(object@y)}
            else {print(object@y[1:10])}
          })