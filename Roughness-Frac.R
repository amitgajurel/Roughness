if(!require("pracma")) install.packages("pracma"); library(pracma)
if(!require("devtools")) install.packages("devtools"); library(devtools)
if(!require("roxygen2")) install.packages("roxygen2"); library(roxygen2)

z <- data.frame(x=1:50, y=rnorm(50))

#' This function calculates the fractal dimension using the
#' modified divider method as detailed in CHARACTERIZATION OF THE ROCK JOINT
#' SURFACE by Vuopio and Polla
#' r refers to the number of points spaced out in the divider method.
#' The values 1 through 20 were chosen when considering the profilometer
#' samples because the samples are taken at 0.05mm, so 1 through 20
#' references spacing of 0.05 mm to 1mm
#' The log_rvals and log_L (x and y axes respectively) are returned for
#' testing purposes.
#' 
#' @param z A dataframe of profile data: length and depth
#' 
#' @author Stephanie Brown, modified for R by D. Craig Jones
#' @examples
#'    z <- data.frame(x=1:50, y=rnorm(50))
#'    divModFrac(z)
divModFrac <- function(z) {
  x <- z$x
  y <- z$y
  
  rv <- seq(6,50,1)# rv = values of r, or the step length
  L <- rep(NULL,length(rv))
  
  for (r in 1:length(rv)) {
  # allows r_vals to be something besides 1:20. Indexing is a little more
  # complicated, but it allows for more flexibility.
    x_down <- x[seq(1,length(x),rv[r])]
    y_down <- y[seq(1,length(y),rv[r])]
  # downsample x and y for hypotenuse calculation. This basically makes
  # "fake" vectors to use when increasing the spacing from 1 to 20 points
    dx <- diff(x_down)
    dy <- diff(y_down)
  # find the differences between the x vectors and the 
  # y vectors in order to use them for the hypotenuse calculations to find
  # the length values
    
    hyp <- rep(NULL, length(dx)) 
    
    for (i in 1:length(dx)) 
      hyp[i] = hypot(dx[i], dy[i]) # makes a vector of hypotenuse values
  
    if (rv[r]>1) {
        last_ind <- (length(x_down) * rv[r])-(rv[r]-1) 
  # find remainder segment, find the last index included in the point
  # method and go to the end)
  # needs to be higher than 0 because then you get a comparison of 1 
  # as last_ind on first run
        if (last_ind < length(x)) {
  # if last_ind is equal to the last index of the original vector, there
  # is no remainder value and these calculations aren't needed. Trying to
  # do them when not needed will cause an index out of bounds error.
          rem_dx <- x[length(x)]-x[last_ind+1]
          rem_dy <- y[length(y)]-y[last_ind+1]
          hyp[i+1] <- hypot(rem_dx, rem_dy)
        }
    }
  # sum the vector of hypotenuse values to find the apparent length, sans remainder.
    L[r] <- sum(hyp) # NOTE: index ONLY works if single spaces of r are used. 
    #for other spacings, reevaluate indexing method
    hyp <- NULL # clear hyp lengths holding vector
  }
  
  log_rvals <- log10(rv) # x-axis
  log_L <- log10(L) # y-axis
  # find best fit coefficients
  coeffs <- polyfit(log_rvals, log_L, 1)
  
  slope_B = coeffs[1]
  intercept = coeffs[2] # some papers suggest that the intercept has meaning
  # for the smaller differences isolates slope from vector for clarity. The 
  # slope is our Beta value.
  
  D = 1-slope_B # D = fractal dimension
  
  return(D)
}