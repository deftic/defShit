#' Calculates FWHM, HRT and AUC of peaks
#'
#' @param z A data.frame containing all peaks as columns and timepoints in column A from a csv file
#'
#' @return The FWHM, HRT and AUC value for the peaks
#' @export
#'
#' @examples
#' test <- fwhm(one)
#' names(test) <- c("xmax", "ymax", "ybase", "halfMax", "xALow", "yALow", "xAHigh", "yAHigh", "xBHigh", "yBHigh", "xBLow", "yBLow", "mA", "mB", "aA", "aB", "xA", "xB", "fwhm", "hrt", "auc")

ttp.fun <- function(z) {
   
   xmax <- findpeaks(z$value, nups = 1, ndowns = 2, npeaks = 1, threshold = 2, sortstr = TRUE)[1, 2] # timepoint with max value within time 4 to 20
   ttp <- xmax - (max(localMinima.fun(z$value[5:xmax])) + 4)  # time to peak
      
   return(ttp)
   
   }

