#.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

if(!require("pracma")) install.packages("pracma"); library(pracma)
if(!require("Rlibeemd")) install.packages("Rlibeemd"); library(Rlibeemd)
if(!require("devtools")) install.packages("devtools"); library(devtools)
if(!require("roxygen2")) install.packages("roxygen2"); library(roxygen2)

#' Calculates the root mean square of a profile
#'
#' @param z A dataframe of profile data: length and depth
#' 
#' @return The RMS of \code{z[,2]}
#' @author Stephanie Brown, modified for R by D. Craig Jones
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
#' @author Stephanie Brown, modified for R by D. Craig Jones
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
#' @author Stephanie Brown, modified for R by D. Craig Jones
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
#' @author Stephanie Brown, modified for R by D. Craig Jones
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


#' Uses a linear chordal approximation to compute the arc length.
#'
#' @param z A dataframe of profile data: length and depth
#'
#' @author Stephanie Brown, modified for R by D. Craig Jones
#'
#' @examples
#'    z <- data.frame(x=1:10, y=rnorm(10))
#'    arclength(z)
arclength <- function(z) {
  seglen <- sqrt(diff(z[,1])^2+diff(z[,2])^2)
  arclen <- sum(seglen)
  
  return(arclen)
}


#' Sinuosity compares the traveled length along the rock
#' (the arclength of the signal) with the lateral length of the ideal line.
#' SINUOSITY_1 USES END TO END LENGTH 
#'
#' @param z A dataframe of profile data: length and depth
#'
#' @author Stephanie Brown, modified for R by D. Craig Jones
#'
#' @examples
#'    z <- data.frame(x=1:10, y=rnorm(10))
#'    SINUOSITY1(z)
SINUOSITY1 <- function(z) {
  x <- z[,1]
  y <- z[,2]
  
  L <- sqrt((x[length(x)]-x[1])^2 + (y[length(x)]-y[1])^2)
  
  arclen <- arclength(z)
  S1 <- arclen/L
  
  return(S1)
  
}


#' Sinuosity compares the traveled length along the rock
#' (the arclength of the signal) with the lateral length of the ideal line.
#' SINUOSITY_2 USES HORIZONTAL LENGTH FROM ADJUSTED SAMPLE END POINTS
#'
#' @param z A dataframe of profile data: length and depth
#'
#' @author Stephanie Brown, modified for R by D. Craig Jones
#'
#' @examples
#'    z <- data.frame(x=1:10, y=rnorm(10))
#'    SINUOSITY2(z)
SINUOSITY2 <- function(z) {
  x <- z[,1]
  
  L <- (x[length(x)]-x[1])
  
  arclen <- arclength(z)
  S2 <- arclen/L
  
  return(S2)
  
}


#' This simply finds the number of turning points.
#'
#' @param z A dataframe of profile data: length and depth
#'
#' @author Stephanie Brown, modified for R by D. Craig Jones
#'
#' @examples
#'    z <- data.frame(x=1:10, y=rnorm(10))
#'    NTurningPoints(z)
NTurningPoints <- function(z) {
  y <- z[,2]
  
  e <- Rlibeemd::extrema(y)
  
  numPoints <- length(e$maxima) + length(e$minima) - 2
  
  return(numPoints)
}


#' Performs the following geometric algorithms:
#' Geometric:: RMS, Sinuosity**, Z2, Energy, MAA, Number of Turning Points
#' 
#' * - Both versions of Sinuosity are included. Sinuosity1 calculated the
#' length by doing the distance formula between first and last point, thus
#' taking elevation differences into account. Sinuosity2 simply subtracts
#' the first x-value from the last x-value, using the length of the line
#' without elevation differences. We could not find in the literature which
#' was more "correct" so we calculated both. In most cases, the difference
#' in calculated roughness was minimal.
#'
#' @param z A dataframe of profile data: length and depth
#'
#' @examples
#'    z <- data.frame(x=1:10, y=rnorm(10))
#'    Geom(z)
Geom <- function(z) {
  retVal <- data.frame(RMS=RMS(z)
                     , ENERGYS=ENERGYS(z)
                     , MAA=MAA(z)
                     , Z2=Z2(z)
                     , SINUOSITY1=SINUOSITY1(z)
                     , SINUOSITY2=SINUOSITY2(z)
                     , NTurningPoints=NTurningPoints(z)
            )
  
  return(retVal)
}