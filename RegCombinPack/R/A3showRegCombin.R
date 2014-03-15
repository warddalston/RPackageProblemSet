#' Showing RegCombin objects
#' 
#' Prints some of the information contained in a RegCombin object into the console
#'
#' @param object an object of class `RegCombin'
#' 
#' @details The show method for objects of class `RegCombin' prints the first ten values from the \code{R2} slot, the first ten values from the \code{y} slot, the leading 5 by 5 matrix from the \code{coefficients} slot and the leading 5 by 5 matrix from the \code{X} slot.  
#' @author Dalston G. Ward
#' @note The print and show methods for objects of class `RegCombin' do NOT return the same output. R^2 and coefficient values are rounded to four decimal places. 
#' @seealso \code{\link{print,RegCombin-method}}
#' @rdname ShowRegCombin
#' @export
setMethod(f="show",
          signature="RegCombin",
          definition=function(object){
            cat("First 10 R^2 values: \n")
            if(length(object@R2) < 10){
              print(round(object@R2,4))
            } else { print(round(object@R2[1:10],4))}
            cat("\n")
            cat("First 5 rows and columns coefficent matrix: \n")
            if(nrow(object@coefficients) < 5 & ncol(object@coefficients) < 5){
              print(round(object@coefficients,4))
            }
            if(nrow(object@coefficients) < 5 & ncol(object@coefficients) >= 5){
              print(round(object@coefficients[,1:5],4))
            }
            if(nrow(object@coefficients) >= 5 & ncol(object@coefficients) < 5){
              print(round(object@coefficients[1:5,],4))
            }
            if(nrow(object@coefficients) >= 5 & ncol(object@coefficients) >= 5){
              print(round(object@coefficients[1:5,1:5],4))
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