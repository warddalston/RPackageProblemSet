#' Printing RegCombin objects
#' 
#' Prints a summary of important information form an object of class `RegCombin'
#' 
#' @usage print(object)
#'
#' @param object an object of class `RegCombin'
#' 
#' @details The print method for objects of class `RegCombin' prints to the console the following information about a `RegCombin' object: the number of observations per regression, the number of regressions run, the number of covariates considered, the highest R^2 value and the variables included in that regression, as well as the average value for each of the included coefficents.  
#'
#' @author Dalston G. Ward: \email{ward.dalston@gmail.com}
#' @note The print and show methods for objects of class `RegCombin' do NOT return the same output.
#' @seealso \code{\link{showRegCombin}}
#' @rdname printRegCombin
#' @export
setMethod(f="print",
          signature="RegCombin",
          definition=function(x,...){
            cat("Information about", substitute(x) , "\n")
            cat("****************************** \n")
            cat("N observations:",length(x@y),"\n")
            cat("N regressions:", ncol(x@coefficients),"\n")
            cat("Covariates considered:", paste(rownames(x@coefficients),collapse=", "),"\n")
            cat("Max R^2:", max(x@R2), "\n")
            cat("Max R^2 Included",names(x@R2)[which.max(x@R2)],"\n")
            cat("Average Coefficeint estimates: \n")
            sapply(1:nrow(x@coefficients),function(y){
              cat("     ", rownames(x@coefficients)[y],": ", mean(x@coefficients[y,],na.rm=T),"\n",sep="")
            })
            cat("****************************** \n")
          })