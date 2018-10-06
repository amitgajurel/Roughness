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

#' Calculates the mean absolute angle, in radians, of input
#' vector x,y.  The data is detrended before calculation.
#'
#' @param z A dataframe of profile data: length and depth
#' 
#' @author D. Craig Jones as modified from matlab by Stephanie Brown
#' @examples
#'    z <- data.frame(x=1:10, y=rnorm(10))
#'    MAA(z)
MAA <- function(z) {
  x <- z[,1]
  y <- z[,2]
  
  # linear detrend
  lm <- polyfit(x,y,1)
  y <- y - (lm[1]*x + lm[2])
  
  MAA <- mean(atan(abs(diff(y)/diff(x))))
  
  return(MAA)
}

#' Calculates the RMS of the first derivative of input vectors x, y
#'
#' @param z A dataframe of profile data: length and depth
#' 
#' @author D. Craig Jones as modified from matlab by Stephanie Brown
#' @examples
#'    z <- data.frame(x=1:10, y=rnorm(10))
#'    Z2(z)
Z2 <- function(z) {
  x <- z[,1]
  y <- z[,2]
  
  n <- length(x)
  z2 <- sqrt((1/n)*sum((diff(y)/diff(x))^2))
  
  return(z2)
  
}
