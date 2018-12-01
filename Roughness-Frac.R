if(!require("pracma")) install.packages("pracma"); library(pracma)
if(!require("devtools")) install.packages("devtools"); library(devtools)
if(!require("roxygen2")) install.packages("roxygen2"); library(roxygen2)

#z <- data.frame(x=1:50, y=rnorm(50))

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


#' RLFRACTAL calculates the fractal dimension of input vectors x and y 
#' using the roughness-length method. The vector wv contains the window
#' lengths of the analysis.
#' 
#' @param z A dataframe of profile data: length and depth
#' 
#' @author Stephanie Brown, modified for R by D. Craig Jones
#' @examples
#'    z <- data.frame(x=1:50, y=rnorm(50))
#'    rlFractal(z)
rlFractal <- function(z) {
    x <- z$x
    y <- z$y
    
    # Global detrending
    a <- polyfit(x,y,1)
    y <- y - (a[1]*x+a[2])
    
    n <- length(x)
    
    wv <- round(linspace(0.04*n, 0.1*n, 7))
    
    nit <- length(wv)
    
    # Calculate window RMS values
    wrms <- rep(0,nit)#zeros(1,nit)
    
    for (k in 1:nit) {
        nw <- floor(n/wv[k])    # size of regular windows
        lwe <- mod(n,wv[k])     # size of off window
        
        # Cycle through windows
        for (j in 1:nw) {
            # calculate RMS for window
            xw <- x[((j-1)*wv[k]+1):(j*wv[k])]
            yw <- y[((j-1)*wv[k]+1):(j*wv[k])]
            
            #Local Detrending
            a <- polyfit(xw,yw,1)
            yw <- yw - (a[1]*xw+a[2])
            
            # Window RMS saved
            wrms[k] <- wrms[k] + sqrt(mean(yw^2))#RMS(yw)
        } # for(j)
        
        # RMS on last (uneven) window
        if (lwe!=0) {
            xw <- x[c((nw*wv[k]+1):length(x))]
            yw <- y[c((nw*wv[k]+1):length(y))]
            
            # Local Detrending
            a <- polyfit(xw,yw,1)
            yw <- yw - (a[1]*xw+a[2])
            
            # RMS updated
            wrms[k] <- wrms[k] + sqrt(mean(yw^2))#RMS(yw)
            wrms[k] <- wrms[k]/(nw+(lwe/wv[k]))
        } else {
            wrms[k] <- wrms[k]/nw
        } # end if
    } # for (k)
    
    Lwv <- log10(wv)
    Lwrms <- log10(wrms)
    
    coeffs <- polyfit(Lwv, Lwrms, 1)
    #D <- 1-coeffs[1]
    D <- 1-coeffs[1]
    
    a <- abs(coeffs[2])
    
    D <- unname(lm(Lwv[use]~Lwrms[use])$coef[1])
    
    return(D)
}

#' This function computes the fractal dimension using the spectral method
#' as detailed in CHARACTERIZATION OF THE ROCK JOINT SURFACE by Vuopio 
#' and Polla
#' 
#' @param z A dataframe of profile data: length and depth
#' 
#' @author Stephanie Brown, modified for R by D. Craig Jones
#' @examples
#'    z <- data.frame(x=1:50, y=rnorm(50))
#'   SpectralMethod(z)
SpectralMethod <- function(z) {
    x <- z$x
    y <- z$y
    
    y <- y-mean(y) # easy way to reduce DC bias
    
    L <- length(x)
    dt <- mean(diff(x))
    Fs <- (1/dt)*10^3
    
    N <- 2^nextpow2(L)
    df <- Fs/N
    f <- seq(0,Fs/2, df)
    
    Y <- fft(y,N)
    Y <- Y[1:(N/2+1)]
    
    psdY <- (1/(N*Fs))*abs(Y)^0.2
    psdY[2:(length(psdY)-1)] <- 2*psdY[2:(length(psdY)-1)]
    
    log_f <- log10(f)
    log_PSD <- log10(psdY)
    
    coeffs <- polyfit(log_f[2:length(log_f)], log_PSD[2:length(log_PSD)],1)
    
    a <- coeffs[2]
    slope_B <- coeffs[1]
    
    D <- 1-(5+slope_B)/2
    
    return(abs(D))
}


semivar <- function(y, h) {
  n <- length(y)
  np <- n-h
  
  smv <- (1/(2*np))*sum((y[1:np]-y[(1:np)+h])^2)

  return(smv)
}

semivarFrac <- function(z) {
    y <- z$y
    
    lagVec <- 1:25
    semivars <- rep(0,length(lagVec))#zeroes(1,length(lagVec))
    for (i in 1:length(lagVec)) {
        semivars[i] <- semivar(y, lagVec[i])
    } # for
    
    logSem <- log10(semivars)
    loglag <- log10(lagVec)
    coeffs <- polyfit(loglag, logSem, 1)
    slope <- coeffs[1]
    a <- coeffs[2]
    D <- 2 - slope/2
    return(D)
}

countSquares <- function(x,y,s) {

    spacing <- x[2]-x[1]
    
    indCount <- s/spacing
    x_pos <- x[1]
    
    q <- 0
    sqCount <- 0
    
    for (i in seq(0,x[length(x)]-x[1]-s,s)) {
        for (j in seq(0,max(y)-min(y),s)) {
            x_pos <- x[1]+i
            y_pos <- min(y)+j
            #pos <- c(x_pos y_pos s s)
            
            indSegY <- y[round(q*indCount+1,0):round((q+1)*indCount,0)]
            indSegX <- x[round(q*indCount+1,0):round((q+1)*indCount,0)]
            
            for (k in 1:indCount) {
                if ( (indSegY[k] > y_pos) && (indSegY[k] < (y_pos+s)) &&
                     (indSegX[k] > x_pos) && (indSegX[k] < (x_pos+s)) ) {
                  sqCount <- sqCount+1
                  break
                } # end if
            } # for(k)
        } # for(j)
        q <- q + 1
    } # for(i)
    
    if ((x_pos+s) < x[length(x)]) {
        x_pos <- x_pos+s
        remStartInd <- round(q*indCount+1,0)
        remSegY <- y[remStartInd:length(y)]
        for (b in seq(0,max(y)-min(y),s)) {
            y_pos <- min(y)+b
            for (a in 1:length(remSegY)) {
                if ( (remSegY[a] > y_pos) && (remSegY[a] < (y_pos+s)) ) {
                    sqCount <- sqCount + 1
                    break
                } # end if
            } # for(a)
        } # for(b)
    } # end if
    
    return(sqCount)
} # countSquares

boxcount <- function(z) {
    x <- z$x
    y <- z$y
    
    R <- c(0.2, 0.4, 0.5, 1, 1.5, 2, 3, 4, 5, 10)
    N <- rep(0,length(R))
    
    for (i in 1:length(R)) {
        N[i] <- countSquares(x, y, R[i])
    } # for(i)
    
    log_N <- log10(N)
    invR <- 1/R
    log_invR <- log10(invR)
    
    use <- is.finite(log_N)
    
    D <- unname(lm(log_invR[use]~log_N[use])$coef[2])
    
    return(D)
} # boxcount


Frac <- function(z) {
  retVal <- data.frame(divModFrac = divModFrac(z)
                       , RoughnessLength = rlFractal(z)
                       , SpectralMethod = SpectralMethod(z)
                       , Semivariance = semivarFrac(z)
                       , boxcount = boxcount(z)
  )
  
  return(retVal)
}
