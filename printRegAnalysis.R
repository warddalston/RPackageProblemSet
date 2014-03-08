#' Printing RegAnalysis objects
#' 
#' Prints a summary of important information form an object of class `RegAnalysis'
#' 
#' @usage print(object)
#'
#' @param object an object of class `RegAnalysis'
#' 
#' @details The print method for objects of class `RegAnalysis' prints to the console the following information about a `RegAnalysis' object: the number of observations per regression, the number of regressions run, the number of covariates considered, the highest R^2 value, the variables included in that regression, the average R^2 value for each regressions including each covariate, the variance of R^2 values for each covariate, as well as the average value for each of the included coefficents.  
#'
#' @author Dalston G. Ward: \email{ward.dalston@gmail.com}
#' @note The print and show methods for objects of class `RegAnalysis' do NOT return the same output.
#' @seealso \code{\link{showRegAnalysis}}
#' @rdname printRegAnalysis
#' @export
setMethod(f="print",
          signature="RegAnalysis",
          definition=function(x,...){
            cat("Information about", substitute(x) , "\n")
            cat("****************************** \n")
            cat("N observations:",length(x@y),"\n")
            cat("N regressions:", ncol(x@coefficients),"\n")
            cat("Covariates considered:", paste(rownames(x@coefficients),collapse=", "),"\n")
            cat("Max R^2:", max(x@R2), "\n")
            cat("Max R^2 Included",names(x@R2)[which.max(x@R2)],"\n")
            cat("Average R^2 estimates: \n")
            sapply(1:length(x@MeanR2),function(y){
              cat("     ", names(x@MeanR2)[y],": ", round(x@MeanR2[y],4),"\n",sep="")
            })
            cat("R^2 estimate variances: \n")
            sapply(1:length(x@VarR2),function(y){
              cat("     ", names(x@VarR2)[y],": ", round(x@VarR2[y],4),"\n",sep="")
            })
            cat("Average Coefficeint estimates: \n")
            sapply(1:nrow(x@coefficients),function(y){
              cat("     ", rownames(x@coefficients)[y],": ", mean(x@coefficients[y,],na.rm=T),"\n",sep="")
            })
            cat("****************************** \n")
          })