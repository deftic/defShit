#' Calculates AUC of peaks
#'
#' @param z A data.frame containing all peaks as columns and timepoints in column 1 from a csv file
#'
#' @return The Area under the Curve
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#' 

area.fun <- function(z) {
   xmax <- pracma::findpeaks(z$value, sortstr = TRUE)[1, 2] # timepoint with max value 
   ybase <- mean(na.omit(z$value[2:5])) # baseline (average of values x = 2 to 4)
   ttp <- xmax - (max(defShit::localMinima.fun(z$value[5:xmax])) + 4)  # time to peak
   xyVals <- subset(z, time >= (xmax - ttp) & time < which.max(z$value <= ybase & z$time > xmax))
   area <- MESS::auc(xyVals[, 1], xyVals[, 2] - mean(z$value[2:5]), type = "spline") # area under the curve
   
   return(area)

   }

