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

area.fun <- function(z) {
   
   area <- auc(z[5:30, 1], z[5:30, 2] - min(z$value), type = "spline") # area under the curve

   return(area)

   }

