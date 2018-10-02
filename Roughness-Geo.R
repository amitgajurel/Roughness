.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

if(!require("pracma")) install.packages("pracma"); library(pracma)
if(!require("devtools")) install.packages("devtools"); library(devtools)
if(!require("roxygen2")) install.packages("roxygen2"); library(roxygen2)

#' Calculates the root mean square of a profile
#'
#' @param z A dataframe of profile data: length and depth
#' 
#' @return The RMS of \code{z[,2]}
#' @author D. Craig Jones as modified from matlab by Stephanie Brown
#' @examples
#'    z <- data.frame(x=1:10, y=rnorm(10))
#'    RMS(z)
RMS <- function(z) {
  e <- z[,2]
  
  #...
  e <- e-mean(e)
  
  return(sqrt(sum(e^2)))
  
}

#' calculates the signal energy of input vectors x and y.
#'
#' @param z A dataframe of profile data: length and depth
#' 
#' @author D. Craig Jones as modified from matlab by Stephanie Brown
#' @examples
#'    z <- data.frame(x=1:10, y=rnorm(10))
#'    ENERGYS(z)
ENERGYS <- function(z) {
  x <- z[,1]
  y <- z[,2]
  
  L <- length(z[,2])
  Es <- (1/L)*pracma::trapz(x,y^2)
  
  return(Es)
}