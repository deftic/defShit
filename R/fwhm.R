#' Calculates FWHM, HRT and AUC of peaks
#'
#' @param z A data.frame containing all peaks as columns and timepoints in column A from a csv file
#'
#' @return The FWHM, HRT and AUC value for the peaks
#' @export
#'
#' @examples
#' test <- fwhm(one)
#' names(test) <- c("xmax", "ymax", "ybase", "halfMax", "xALow", "yALow", "xAHigh", "yAHigh", "xBHigh", "yBHigh", "xBLow", "yBLow", "mA", "mB", "aA", "aB", "xA", "xB", "fwhm", "hrt", "ttp", "area")

fwhm <- function(z) {
  data.frame(
      xmax <- z$time[z$value==max(z$value[4:20])], # timepoint with max value within time 4 to 20
      ymax <- z$value[xmax], # max value
      ybase <- mean(na.omit(z$value[2:4])), # baseline (average of values x = 2 to 4)
      ypeak <- (ymax - ybase), # max value without baseline
      halfMax <- (((ymax - ybase) / 2) + ybase), # half way between baseline and peak max

      xAHigh <- which.max(z$value >= halfMax), # second ascending point (A)
      xALow <- xAHigh - 1, # first ascending point (A)
      yAHigh <- z$value[xAHigh],
      yALow <- z$value[xALow],

      xBHigh <- max(which(z$value >= halfMax & z$time <= 30)), # first descending point (B)
      xBLow <- xBHigh + 1, # second descending fwhm point (B)
      yBHigh <- z$value[xBHigh],
      yBLow <- z$value[xBLow],

      # coefficients for regression ; y = mx + a ; x = (y - a) / m ; a = y - mx
      mA <- (yAHigh - yALow) / (xAHigh - xALow), # slope of point A
      aA <- (yALow * xAHigh - yAHigh * xALow) / (xAHigh - xALow), # intercept of point A
      xA <- (halfMax - aA) / mA,

      mB <- (yBHigh - yBLow) / (xBHigh - xBLow), # slope of point B
      aB <- (yBLow * xBHigh - yBHigh * xBLow) / (xBHigh - xBLow), # intercept of point B
      xB <- (halfMax - aB) / mB,

      fwhm <- xB - xA,  # full width at half maximum
      hrt <- xB - xmax, # half relaxation time
      area <- auc(z[, 1], z[, 2] - ybase, type = "spline") # area under the curve
    )
}

