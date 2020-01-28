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
   
   area <- MESS::auc(z[5:30, 1], z[5:30, 2] - min(z$value), type = "spline") # area under the curve

   return(area)

   }

