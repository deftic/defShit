#' Calculates FWHM of peaks
#'
#' @param z A data.frame containing all peaks as columns and timepoints in column A from a csv file
#'
#' @return The FWHM value for the peak
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#' 

fwhm.fun <- function(z) {
      xmax <- pracma::findpeaks(z$value[4:20], sortstr = TRUE)[1, 2] # timepoint with max value 
      ymax <- z$value[xmax] # max value
      ybase <- mean(na.omit(z$value[2:5])) # baseline (average of values x = 2 to 4)
      ypeak <- (ymax - ybase) # max value without baseline
      halfMax <- (((ymax - ybase) / 2) + ybase) # half way between baseline and peak max

      xAHigh <- which.max(z$value[2:xmax] >= halfMax) + 1 # second ascending point (A)
      xALow <- xAHigh - 1 # first ascending point (A)
      yAHigh <- z$value[xAHigh]
      yALow <- z$value[xALow]

      xBHigh <- max(which(z$value >= halfMax & z$time <= 35)) # first descending point (B)
      xBLow <- xBHigh + 1 # second descending fwhm point (B)
      yBHigh <- z$value[xBHigh]
      yBLow <- z$value[xBLow]

      # coefficients for regression ; y = mx + a ; x = (y - a) / m ; a = y - mx
      mA <- (yAHigh - yALow) / (xAHigh - xALow) # slope of point A
      aA <- (yALow * xAHigh - yAHigh * xALow) / (xAHigh - xALow) # intercept of point A
      xA <- (halfMax - aA) / mA

      mB <- (yBHigh - yBLow) / (xBHigh - xBLow) # slope of point B
      aB <- (yBLow * xBHigh - yBHigh * xBLow) / (xBHigh - xBLow) # intercept of point B
      xB <- (halfMax - aB) / mB
      
      fwhm <- xB - xA  # full width at half maximum

      return(fwhm)
}

